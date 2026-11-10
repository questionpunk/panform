-- | Tidy logic that was derived rather than written, such as show-if
-- conditions rebuilt from jump rules. Equality tests on one question are
-- treated as a set of allowed values:
--
-- > (seed = '1' or seed = '2') and seed != '1'   ==>   seed = '2'
--
-- An expression with nothing to tidy comes back unchanged.
module Panform.Expr.Simplify (simplify, simplifyLogic) where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Panform.AST

simplify :: Expr -> Expr
simplify e = let e' = step e in if e' == e then e else simplify e'

step :: Expr -> Expr
step = \case
  ENot (ENot x) -> step x
  ENot (ELit (LBool b)) -> ELit (LBool (not b))
  ENot x -> ENot (step x)
  e@(EBin And _ _) -> junction And (map step (flatten And e))
  e@(EBin Or _ _) -> junction Or (map step (flatten Or e))
  EBin o a b -> EBin o (step a) (step b)
  ECall f xs -> ECall f (map step xs)
  e -> e

flatten :: BinOp -> Expr -> [Expr]
flatten op (EBin o a b) | o == op = flatten op a ++ flatten op b
flatten _ e = [e]

-- | Rebuild a flat and/or, folding constants, dropping repeats and merging
-- the value sets of each question.
junction :: BinOp -> [Expr] -> Expr
junction op xs
  | any (== absorbing) xs = absorbing
  | otherwise = case nub (mergeSets op (filter (/= unit) xs)) of
      [] -> unit
      ys -> foldl1 (EBin op) ys
  where
    unit = ELit (LBool (op == And))
    absorbing = ELit (LBool (op == Or))

-- | A test of one question against a set of values.
data Test = In Text [Lit] | NotIn Text [Lit]

asTest :: Expr -> Maybe Test
asTest = \case
  EBin Eq (ERef q) (ELit v) -> Just (In q [v])
  EBin Neq (ERef q) (ELit v) -> Just (NotIn q [v])
  EBin Or a b -> do
    In q vs <- asTest a
    In q' ws <- asTest b
    if q == q' then Just (In q (nub (vs ++ ws))) else Nothing
  _ -> Nothing

fromTest :: Test -> Expr
fromTest = \case
  In _ [] -> ELit (LBool False)
  In q vs -> foldl1 (EBin Or) [EBin Eq (ERef q) (ELit v) | v <- vs]
  NotIn _ [] -> ELit (LBool True)
  NotIn q vs -> foldl1 (EBin And) [EBin Neq (ERef q) (ELit v) | v <- vs]

-- | Under "and", a question's allowed set is the intersection of its
-- positive tests minus its negative ones. Under "or", positive tests on
-- the same question unite. Only questions tested more than once change.
mergeSets :: BinOp -> [Expr] -> [Expr]
mergeSets op xs = go xs
  where
    counts = M.fromListWith (+) [(q, 1 :: Int) | Just t <- map asTest xs, let q = key t]
    key (In q _) = q
    key (NotIn q _) = q
    repeated q = M.findWithDefault 0 q counts > 1
    go [] = []
    go (x : rest) = case asTest x of
      Just t | repeated (key t) ->
        let q = key t
            (same, others) = partitionTests q rest
         in combine op (t : same) : go others
      _ -> x : go rest
    partitionTests q = foldr pick ([], [])
      where
        pick y (s, o) = case asTest y of
          Just t | key t == q -> (t : s, o)
          _ -> (s, y : o)

combine :: BinOp -> [Test] -> Expr
combine And ts =
  let q = case ts of
        (In n _ : _) -> n
        (NotIn n _ : _) -> n
        [] -> ""
      ins = [vs | In _ vs <- ts]
      outs = concat [vs | NotIn _ vs <- ts]
   in case ins of
        [] -> fromTest (NotIn q (nub outs))
        (v : vs) -> fromTest (In q [x | x <- foldr intersect' v vs, x `notElem` outs])
  where
    intersect' a b = [x | x <- b, x `elem` a]
combine _ ts = case [vs | In _ vs <- ts] of
  vss | length vss == length ts, (In q _ : _) <- ts -> fromTest (In q (nub (concat vss)))
  _ -> foldl1 (EBin Or) (map fromTest ts)

-- | Simplify every condition in a survey.
simplifyLogic :: Form -> Form
simplifyLogic (Form m is) = Form m (map item is)
  where
    item = \case
      ItemGroup g -> ItemGroup g {groupRelevant = simplify <$> groupRelevant g, groupItems = map item (groupItems g)}
      ItemQuestion q -> ItemQuestion q {qRelevant = simplify <$> qRelevant q}
      ItemNote i t r -> ItemNote i t (simplify <$> r)
      i -> i
