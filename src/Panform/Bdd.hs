-- | Conditions in canonical form, as reduced ordered decision diagrams.
-- Two conditions with the same meaning get the same diagram, and so print
-- as the same expression; converting jump rules to show-if and back
-- therefore reaches a fixed point instead of growing or reshuffling the
-- expression on every pass.
--
-- Equality tests on one question share a multi-way node (the answer is
-- one value, or none of those tested), so @q = 'a' and q != 'b'@ is the
-- same condition as @q = 'a'@. Every other test is a two-way node.
-- Nodes are ordered by an intrinsic key (the referenced question's
-- position, then the test's text), never by first appearance, so the
-- order does not depend on which conditions happen to be converted.
module Panform.Bdd
  ( Bdd
  , Node
  , runBdd
  , false
  , true
  , fromExpr
  , toExpr
  , toExprWithin
  , bAnd
  , bOr
  , bNot
  , bAnds
  , bOrs
  ) where

import Control.Monad (foldM)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST

-- | A diagram node; 0 is false and 1 is true.
type Node = Int

-- | Variable order key: question position, then the variable's text.
type Key = (Int, Text)

-- | What a variable tests: one question's value against literals, or a
-- single condition that is true or false.
data Var = ValueOf Text | Test Expr

-- | A variable, the child for each branch value, and the child otherwise.
type Shape = (Key, [(Text, Node)], Node)

data St = St
  { stNodes :: IM.IntMap Shape
  , stUnique :: M.Map Shape Node
  , stVars :: M.Map Key Var
  , stLits :: M.Map (Key, Text) Lit
  , stMemo :: M.Map (Int, Node, Node) Node
  , stPos :: Text -> Int
  , stNext :: Int
  -- ^ The next node number (counting the map's size each time is linear).
  }

type Bdd = State St

-- | Run with a function giving each question's position in the form.
runBdd :: (Text -> Int) -> Bdd a -> a
runBdd pos m = evalState m (St IM.empty M.empty M.empty M.empty M.empty pos 2)

false, true :: Node
false = 0
true = 1

mk :: Key -> M.Map Text Node -> Node -> Bdd Node
mk k bs d =
  let bs' = M.toAscList (M.filter (/= d) bs)
   in if null bs'
        then pure d
        else do
          u <- gets stUnique
          case M.lookup (k, bs', d) u of
            Just n -> pure n
            Nothing -> do
              n <- gets stNext
              modify' $ \s -> s {stNodes = IM.insert n (k, bs', d) (stNodes s), stUnique = M.insert (k, bs', d) n u, stNext = n + 1}
              pure n

shape :: Node -> Bdd (Maybe Shape)
shape n = gets (IM.lookup n . stNodes)

memo :: Int -> Node -> Node -> Bdd Node -> Bdd Node
memo op a b go = do
  m <- gets stMemo
  case M.lookup (op, a, b) m of
    Just r -> pure r
    Nothing -> do
      r <- go
      modify' $ \s -> s {stMemo = M.insert (op, a, b) r (stMemo s)}
      pure r

bNot :: Node -> Bdd Node
bNot 0 = pure true
bNot 1 = pure false
bNot a = memo 0 a a $ shape a >>= \case
  Just (k, bs, d) -> do
    bs' <- traverse bNot (M.fromList bs)
    d' <- bNot d
    mk k bs' d'
  Nothing -> pure a

-- | And (op 1) or or (op 2), by expanding the smaller variable.
apply :: Int -> Node -> Node -> Bdd Node
apply op a b
  | op == 1 && (a == 0 || b == 0) = pure false
  | op == 1 && a == 1 = pure b
  | op == 1 && b == 1 = pure a
  | op == 2 && (a == 1 || b == 1) = pure true
  | op == 2 && a == 0 = pure b
  | op == 2 && b == 0 = pure a
  | a == b = pure a
  | otherwise = memo op (min a b) (max a b) $ do
      sa <- shape a
      sb <- shape b
      let k = minimum [kk | Just (kk, _, _) <- [sa, sb]]
          cof self = \case
            Just (kk, bs, d) | kk == k -> (M.fromList bs, d)
            _ -> (M.empty, self)
          (abs', ad) = cof a sa
          (bbs, bd) = cof b sb
          vals = M.keys (M.union abs' bbs)
      bs <- M.fromList <$> mapM (\v -> (,) v <$> apply op (M.findWithDefault ad v abs') (M.findWithDefault bd v bbs)) vals
      d <- apply op ad bd
      mk k bs d

bAnd, bOr :: Node -> Node -> Bdd Node
bAnd = apply 1
bOr = apply 2

bAnds, bOrs :: [Node] -> Bdd Node
bAnds = foldM bAnd true
bOrs = foldM bOr false

-- | A test as a variable. @!=@, @>=@ and @<=@ are the negations of @=@,
-- @<@ and @>@.
atom :: Expr -> Bdd Node
atom e = case e of
  EBin Neq a b -> atom (EBin Eq a b) >>= bNot
  EBin Ge a b -> atom (EBin Lt a b) >>= bNot
  EBin Le a b -> atom (EBin Gt a b) >>= bNot
  EBin Eq (ERef q) (ELit l) -> do
    pos <- gets stPos
    let k = (pos q, "=" <> q <> "#" <> kind l)
        v = litKey l
    modify' $ \s -> s {stVars = M.insert k (ValueOf q) (stVars s), stLits = M.insert (k, v) l (stLits s)}
    mk k (M.singleton v true) false
  _ -> do
    pos <- gets stPos
    let k = (maybe maxBound pos (firstRef e), T.pack (show e))
    modify' $ \s -> s {stVars = M.insert k (Test e) (stVars s)}
    mk k (M.singleton "" true) false
  where
    kind = \case
      LStr _ -> "s"
      LNum _ -> "n"
      LBool _ -> "b"
    litKey = T.pack . show

firstRef :: Expr -> Maybe Text
firstRef = \case
  ERef q -> Just q
  ESelected q _ -> Just q
  EAnswered q -> Just q
  ENot x -> firstRef x
  EBin _ a b -> maybe (firstRef b) Just (firstRef a)
  ECall _ xs -> case [q | Just q <- map firstRef xs] of
    (q : _) -> Just q
    [] -> Nothing
  _ -> Nothing

fromExpr :: Expr -> Bdd Node
fromExpr e = case e of
  ELit (LBool b) -> pure (if b then true else false)
  ENot x -> fromExpr x >>= bNot
  EBin And a b -> do
    x <- fromExpr a
    y <- fromExpr b
    bAnd x y
  EBin Or a b -> do
    x <- fromExpr a
    y <- fromExpr b
    bOr x y
  _ -> atom e

-- | The diagram as an expression. A node's branches are grouped by the
-- child they lead to, in the order of their values:
--
-- > (q = 'a' or q = 'b') and <child>    -- otherwise false
-- > (q != 'a' and q != 'b') or <child>  -- otherwise true
-- > q = 'a' or (q != 'b' and <otherwise>)  -- a: true, b: false
toExpr :: Node -> Bdd Expr
toExpr 0 = pure (ELit (LBool False))
toExpr 1 = pure (ELit (LBool True))
toExpr n =
  shape n >>= \case
    Nothing -> pure (ELit (LBool True))
    Just (k, bs, d) -> do
      var <- gets (M.lookup k . stVars)
      lits <- gets stLits
      let groups = M.elems (M.fromListWith (flip (++)) [(c, [v]) | (v, c) <- bs])
          byFirst = sortOn (\vs -> minimum vs) groups
          childOf vs = case lookup' vs of
            Just c -> c
            Nothing -> d
          lookup' vs = case vs of
            (v : _) -> lookup v bs
            [] -> Nothing
          test positive vs = case var of
            Just (ValueOf q) ->
              let eqs = [(if positive then EBin Eq else EBin Neq) (ERef q) (ELit l) | v <- vs, Just l <- [M.lookup (k, v) lits]]
               in foldl1' (if positive then EBin Or else EBin And) eqs
            Just (Test e) -> if positive then e else negTest e
            Nothing -> ELit (LBool positive)
      case d of
        0 -> orAll <$> mapM (\vs -> andE (test True vs) <$> toExpr (childOf vs)) byFirst
        1 -> andAll <$> mapM (\vs -> orE (test False vs) <$> toExpr (childOf vs)) byFirst
        _ -> do
          -- Values leading to false add nothing; values leading to true
          -- need not be excluded from the "otherwise" case.
          let live = [vs | vs <- byFirst, childOf vs /= false]
              guarded = [v | (v, c) <- bs, c /= true]
          ins <- mapM (\vs -> andE (test True vs) <$> toExpr (childOf vs)) live
          rest <- toExpr d
          let other = if null guarded then rest else andE (test False guarded) rest
          pure (orAll (ins ++ [other]))
  where
    foldl1' f = \case
      (x : xs) -> foldl f x xs
      [] -> ELit (LBool False)
    andE a b = case b of
      ELit (LBool True) -> a
      ELit (LBool False) -> b
      _ -> EBin And a b
    orE a b = case b of
      ELit (LBool False) -> a
      ELit (LBool True) -> b
      _ -> EBin Or a b
    orAll = \case
      [] -> ELit (LBool False)
      (x : xs) -> foldl (EBin Or) x xs
    andAll = \case
      [] -> ELit (LBool True)
      (x : xs) -> foldl (EBin And) x xs

-- | 'toExpr', unless the expression could exceed the given number of
-- nodes. A diagram shares its sub-diagrams; an expression repeats them, so
-- a small diagram can stand for an enormous expression.
toExprWithin :: Int -> Node -> Bdd (Maybe Expr)
toExprWithin limit n = do
  (sz, _) <- bound IM.empty n
  if sz > limit then pure Nothing else Just <$> toExpr n
  where
    cap = min (limit + 1)
    -- An upper bound on the size of 'toExpr' of a node: per branch value
    -- its test and child, the test again to guard the otherwise case, and
    -- the otherwise child.
    bound memo x
      | x < 2 = pure (1, memo)
      | Just v <- IM.lookup x memo = pure (v, memo)
      | otherwise =
          shape x >>= \case
            Nothing -> pure (1, memo)
            Just (k, bs, d) -> do
              var <- gets (M.lookup k . stVars)
              let t = case var of
                    Just (Test e) -> exprSize e + 2
                    _ -> 4
              (kids, memo') <- foldM (\(acc, m) c -> (\(v, m') -> (cap (acc + v + 2 * t), m')) <$> bound m c) (0, memo) (map snd bs)
              (dv, memo'') <- bound memo' d
              let v = cap (kids + dv + 4)
              pure (v, IM.insert x v memo'')
    exprSize = \case
      ENot y -> 1 + exprSize y
      EBin _ a b -> 1 + exprSize a + exprSize b
      ECall _ xs -> 1 + sum (map exprSize xs)
      _ -> 1

negTest :: Expr -> Expr
negTest = \case
  EBin Lt a b -> EBin Ge a b
  EBin Gt a b -> EBin Le a b
  x -> ENot x
