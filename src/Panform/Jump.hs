-- | Jump ("skip to") logic versus show-if conditions. Formats such as
-- Formbricks and Tally route with "if C, jump to screen k"; the IR asks
-- "show this screen when R". A forward jump from screen @i@ to @k@ on @C@
-- hides every screen strictly between them when @C@ holds; a chain of
-- "if R(i+1) go to i+1, if R(i+2) go to i+2, ..., else go to u" is the
-- exact encoding of per-screen conditions, which writers emit and readers
-- recognise.
module Panform.Jump
  ( Rule (..)
  , jumpsToRelevance
  , jumpsToRelevanceApprox
  , relevanceToJumps
  , neg
  , conj
  , disj
  ) where

import Control.Monad (foldM, forM, forM_, when)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Bdd (Bdd, Node, bAnd, bAnds, bNot, bOrs, fromExpr, runBdd, toExprWithin, true)
import Panform.Conv

-- | "When the condition holds, go to screen @ruleTarget@." Screens are
-- numbered from 0; the screen count means "the end of the survey".
data Rule = Rule
  { ruleCond :: Expr
  , ruleTarget :: Int
  }
  deriving (Eq, Show)

neg :: Expr -> Expr
neg (ENot x) = x
neg x = ENot x

conj :: [Expr] -> Maybe Expr
conj [] = Nothing
conj (x : xs) = Just (foldl (EBin And) x xs)

disj :: [Expr] -> Maybe Expr
disj [] = Nothing
disj (x : xs) = Just (foldl (EBin Or) x xs)

-- | The largest show-if condition, in expression nodes, that
-- 'jumpsToRelevance' keeps for one screen, and the most (jump, screen it
-- passes over) pairs it considers. A condition built term by term beyond
-- the first bound is rebuilt in canonical form; a screen whose canonical
-- condition is still too large, or jump logic beyond the second bound,
-- gets no condition from the jumps, with a warning.
maxNodes, maxPairs, compactAt :: Int
maxNodes = 20000
maxPairs = 200000
compactAt = 500

-- | Screen conditions implied by each screen's jump rules and fallback
-- target. Backward jumps cannot become show-if conditions and are dropped
-- with a warning naming the screen.
--
-- A screen is shown unless a jump passes over it, and a jump only happens
-- on a screen that was itself shown: every "skipped by the jump from @i@"
-- term carries the condition derived for screen @i@. (The part of that
-- condition already implied by another jump over the same screen is left
-- out.) The writer's own chains (see 'relevanceToJumps') read back as the
-- conditions they were written from, when every screen of the chain
-- continues it and no other jump lands inside it.
jumpsToRelevance :: (Int -> Text) -> [([Rule], Maybe Int)] -> Conv [Maybe Expr]
jumpsToRelevance name units = fst <$> jumpsToRelevanceApprox name units

-- | 'jumpsToRelevance', with the screens whose condition was too large to
-- keep and was left out (each named in a warning).
jumpsToRelevanceApprox :: (Int -> Text) -> [([Rule], Maybe Int)] -> Conv ([Maybe Expr], [Int])
jumpsToRelevanceApprox name units = do
  forM_ (zip [0 ..] units) $ \(i, (rs, fb)) -> do
    forM_ rs $ \r -> when (ruleTarget r <= i) $ warn (name i) "backward jump dropped"
    forM_ fb $ \f -> when (f <= i) $ warn (name i) "backward fallback jump dropped"
  if pairs > maxPairs
    then do
      warn "jumps" ("jump logic passes over more than " <> T.pack (show maxPairs) <> " screens in total; no show-if conditions derived from it")
      -- The screens some jump passes over (without listing the pairs).
      let reach = M.fromListWith max [(i, k) | (i, k, _) <- jumps]
          furthest = scanl max 0 [M.findWithDefault 0 i reach | i <- [0 .. n - 1]]
      pure (map (const Nothing) units, [j | (j, m) <- zip [0 .. n - 1] furthest, j < m])
    else do
      -- Built term by term, a condition repeats the conditions of the
      -- screens before it. A large one is replaced by its canonical form
      -- when that is smaller.
      let large = [j | j <- [0 .. n - 1], maybe False (exceeds compactAt) (rel j)]
          canonical = if null large then M.empty else canonicalRels
          fits = not . maybe False (exceeds maxNodes)
      results <- forM [0 .. n - 1] $ \j -> do
        let built = rel j
            canon = M.findWithDefault (Left ()) j canonical
            smaller c = not (fits built) || maybe 0 size c < maybe 0 size built
        case canon of
          _ | j `notElem` large -> pure (built, False)
          Right c | smaller c -> pure (c, False)
          _ | fits built -> pure (built, False)
          _ -> do
            warn (name j) ("show-if condition from the jumps needs more than " <> T.pack (show maxNodes) <> " terms; left out, the screen is shown whenever it is reached")
            pure (Nothing, True)
      pure (map fst results, [j | (j, (_, True)) <- zip [0 ..] results])
  where
    n = length units
    unitAt = M.fromList (zip [0 ..] units)
    -- The earliest screen with a forward jump landing on each screen.
    landFrom =
      M.fromListWith min [(k, i) | (i, (rs, fb)) <- zip [0 ..] units, k <- map ruleTarget rs ++ maybe [] pure fb, k > i]
    -- A chain as the writer emits it, headed by screen i: i tries each of
    -- the next m screens in turn and falls back to the one after them, every
    -- screen inside does the same for the rest, and no jump from before i
    -- lands inside. Those m screens then have exactly the rule conditions.
    chainAt i = case M.lookup i unitAt of
      Just (rs, fb)
        | m <- length rs
        , m > 0
        , i + m < n
        , map ruleTarget rs == [i + 1 .. i + m]
        , fb == Just (i + m + 1)
        , and [M.lookup (i + s) unitAt == Just (drop s rs, fb) | s <- [1 .. m - 1]]
        , all ((>= i) . snd) (takeWhile ((<= i + m) . fst) (M.toAscList (snd (M.split i landFrom)))) ->
            Just (m, rs)
      _ -> Nothing
    chains = scan 0
      where
        scan i
          | i >= n = []
          | Just (m, rs) <- chainAt i = (i, m, rs) : scan (i + m)
          | otherwise = scan (i + 1)
    memberCond = M.fromList [(i + s, ruleCond r) | (i, _, rs) <- chains, (s, r) <- zip [1 ..] rs]
    inChain = S.fromList [i + s | (i, m, _) <- chains, s <- [0 .. m - 1]]
    -- Every other forward jump: (source, target, condition under which the
    -- source takes it).
    jumps =
      [ (i, min k n, t)
      | (i, (rs, fb)) <- zip [0 ..] units
      , not (S.member i inChain)
      , (k, t) <- ruleSkips rs ++ fallbackSkip rs fb
      , k > i + 1
      ]
    pairs = sum [k - i - 1 | (i, k, _) <- jumps]
    -- The jumps passing over each screen, in document order.
    over = M.map reverse (M.fromListWith (++) [(j, [x]) | x@(i, k, _) <- jumps, j <- [i + 1 .. k - 1]])
    firstLanding = M.map (minimum . map (\(_, k, _) -> k)) over
    ruleSkips rs =
      [ (ruleTarget r, fromMaybe (ruleCond r) (conj (ruleCond r : map (neg . ruleCond) earlier)))
      | (m, r) <- zip [0 ..] rs
      , let earlier = take m rs
      ]
    fallbackSkip rs fb = case fb of
      Just f -> [(f, fromMaybe (ELit (LBool True)) (conj (map (neg . ruleCond) rs)))]
      Nothing -> []
    rels = ML.fromList [(j, relOf j) | j <- [0 .. n - 1]]
    rel j = ML.findWithDefault Nothing j rels
    relOf j =
      conj $
        maybe [] pure (M.lookup j memberCond)
          ++ maybe [] (pure . neg) (disj [fromMaybe t (conj (shown i j ++ [t])) | (i, _, t) <- M.findWithDefault [] j over])
    -- That screen i is shown, given the other jumps over j: the jumps over
    -- i that also pass over j hide j themselves and need no mention.
    shown i j =
      maybe [] pure (M.lookup i memberCond)
        ++ [ neg (maybe t (\r -> EBin And r t) (rel h))
           | M.findWithDefault maxBound i firstLanding <= j
           , (h, k, t) <- M.findWithDefault [] i over
           , k <= j
           ]
    -- The same conditions as 'rel', as canonical decision diagrams.
    canonicalRels = runBdd (\q -> M.findWithDefault maxBound q firstUse) $ do
      nodes <- foldM (\acc j -> (\r -> M.insert j r acc) <$> relB acc j) M.empty [0 .. n - 1]
      -- 'Left' when even the canonical form is too large.
      forM nodes $ \r -> if r == true then pure (Right Nothing) else maybe (Left ()) (Right . Just) <$> toExprWithin maxNodes r
    -- Questions in the order the jumps first test them, which keeps the
    -- diagrams of a survey read front to back small.
    firstUse = M.fromListWith min [(q, i) | (i, (rs, _)) <- zip [0 :: Int ..] units, r <- rs, q <- refsOf (ruleCond r)]
    refsOf = \case
      ERef q -> [q]
      ESelected q _ -> [q]
      EAnswered q -> [q]
      ENot x -> refsOf x
      EBin _ a b -> refsOf a ++ refsOf b
      ECall _ xs -> concatMap refsOf xs
      _ -> []
    relB :: M.Map Int Node -> Int -> Bdd Node
    relB acc j = do
      member <- maybe (pure true) fromExpr (M.lookup j memberCond)
      terms <- forM (M.findWithDefault [] j over) $ \(i, _, t) -> do
        sh <- shownB acc i j
        tn <- fromExpr t
        bAnds (sh ++ [tn])
      hidden <- bOrs terms
      bNot hidden >>= bAnd member
    shownB acc i j = do
      member <- maybe (pure []) (fmap pure . fromExpr) (M.lookup i memberCond)
      others <-
        sequence
          [ fromExpr t >>= bAnd (M.findWithDefault true h acc) >>= bNot
          | M.findWithDefault maxBound i firstLanding <= j
          , (h, k, t) <- M.findWithDefault [] i over
          , k <= j
          ]
      pure (member ++ others)
    size :: Expr -> Int
    size = \case
      ENot y -> 1 + size y
      EBin _ l r -> 1 + size l + size r
      _ -> 1
    exceeds limit e = go limit [e]
      where
        go :: Int -> [Expr] -> Bool
        go b _ | b < 0 = True
        go _ [] = False
        go b (x : xs) = case x of
          ENot y -> go (b - 1) (y : xs)
          EBin _ l r -> go (b - 1) (l : r : xs)
          _ -> go (b - 1) xs

-- | The exact jump encoding of per-screen conditions: from screen @i@, try
-- each following conditional screen in turn and fall back to the first
-- unconditional one (or the end). The first screen's condition has no
-- screen before it to jump from; the caller warns about it.
relevanceToJumps :: [Maybe Expr] -> [([Rule], Maybe Int)]
relevanceToJumps rels = [go i | i <- [0 .. n - 1]]
  where
    n = length rels
    indexed = zip [0 ..] rels
    go i =
      let following = drop (i + 1) indexed
          conds = [(j, e) | (j, Just e) <- takeWhile (isJust . snd) following]
          u = i + 1 + length conds
       in if null conds then ([], Nothing) else ([Rule e j | (j, e) <- conds], Just u)
