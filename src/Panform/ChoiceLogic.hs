-- | Helpers shared by the market-research formats (Decipher, Dimensions,
-- Blaise). Their logic tests a closed question by category name rather
-- than by comparing an answer to a string, and their category names must be
-- identifiers, so writers need a name for every choice value and readers
-- need the value back.
module Panform.ChoiceLogic
  ( ChoiceIndex
  , choiceIndex
  , boolChoices
  , litValue
  , toSelected
  , fromSelected
  , mapExpr
  , andMaybe
  , isIdent
  , isInteger
  , codeName
  , scaleChoices
  , asScale
  , surveyLangs
  , warnTranslations
  , flattenGroups
  , mapSurveyLogic
  , legalIds
  , dropUnprintable
  ) where

import Control.Monad (forM_, unless)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import Panform.AST
import Panform.Conv
import Panform.Expr (Dialect (..), printExpr)

-- | Closed questions by id: selection and choices. Booleans count as a
-- single choice between @yes@ and @no@ when the format has no boolean.
type ChoiceIndex = M.Map Text (Selection, [Choice])

choiceIndex :: Bool -> Form -> ChoiceIndex
choiceIndex withBool s =
  M.fromList $
    [(qId q, (sel, cs)) | q <- questions s, QChoice sel _ cs <- [qType q]]
      ++ [(qId q, (Single, boolChoices)) | withBool, q <- questions s, QBoolean <- [qType q]]
      ++ [(qId q, (Multiple, cs)) | q <- questions s, QRanking cs <- [qType q]]

boolChoices :: [Choice]
boolChoices = [choice "yes" "Yes", choice "no" "No"]

-- | A literal as a choice value; @true@ is the @yes@ of 'boolChoices'.
litValue :: Lit -> Text
litValue = \case
  LStr t -> t
  LNum n
    | n == fromIntegral (round n :: Integer) -> T.pack (show (round n :: Integer))
    | otherwise -> T.pack (showFFloat Nothing n "")
  LBool b -> if b then "yes" else "no"

-- | Bottom-up rewrite.
mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f = go
  where
    go e = f $ case e of
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall n xs -> ECall n (map go xs)
      _ -> e

-- | Writer side: an equality test on a closed question becomes a category
-- test, and every category test uses the name the format gives the value.
toSelected :: ChoiceIndex -> (Text -> Text -> Text) -> Expr -> Expr
toSelected idx name = mapExpr $ \case
  EBin Eq (ERef q) (ELit l) | M.member q idx -> ESelected q (name q (litValue l))
  EBin Neq (ERef q) (ELit l) | M.member q idx -> ENot (ESelected q (name q (litValue l)))
  ESelected q v -> ESelected q (name q v)
  e -> e

-- | Reader side: category names back to values, and a category test on a
-- single-choice question back to an equality.
fromSelected :: ChoiceIndex -> (Text -> Text -> Text) -> Expr -> Expr
fromSelected idx value = mapExpr $ \case
  ESelected q n -> case M.lookup q idx of
    Just (Single, _) -> EBin Eq (ERef q) (ELit (LStr (value q n)))
    _ -> ESelected q (value q n)
  ENot (EBin Eq a b) -> EBin Neq a b
  e -> e

andMaybe :: Maybe Expr -> Maybe Expr -> Maybe Expr
andMaybe (Just a) (Just b) = Just (EBin And a b)
andMaybe a Nothing = a
andMaybe Nothing b = b

isIdent :: Text -> Bool
isIdent t = case T.uncons t of
  Just (c, rest) -> isAlpha c && T.all (\x -> isAlphaNum x || x == '_') rest
  Nothing -> False

isInteger :: Text -> Bool
isInteger t = not (T.null t) && T.all isDigit t

-- | An identifier for a choice value: the value itself when it is one,
-- else the prefix followed by the value with other characters replaced.
codeName :: Text -> Text -> Text
codeName prefix v
  | isIdent v = v
  | otherwise = prefix <> T.map (\c -> if isAlphaNum c || c == '_' then c else '_') v

-- | A scale as numbered choices; the end points carry the end labels.
scaleChoices :: Int -> Int -> Maybe I18n -> Maybe I18n -> [Choice]
scaleChoices lo hi l h =
  [ Choice (tshow n) (lbl n) False False
  | n <- [lo .. hi]
  , let lbl k
          | k == lo, Just t <- l = t
          | k == hi, Just t <- h = t
          | otherwise = plain (tshow k)
  ]

-- | The inverse of 'scaleChoices': consecutive integer values whose inner
-- labels are just the numbers.
asScale :: [Choice] -> Maybe QType
asScale cs = case mapM (readInt . choiceValue) cs of
  Just ns@(lo : _ : _ : _)
    | ns == [lo .. lo + length ns - 1]
    , not (any (\c -> choiceOther c || choiceExclusive c) cs)
    , and [choiceLabel c == plain (tshow n) | (c, n) <- inner (zip cs ns)] ->
        let hi = lo + length ns - 1
            end c n = if choiceLabel c == plain (tshow n) then Nothing else Just (choiceLabel c)
            (c0, cN) = (head' cs, last' cs)
         in Just (QScale lo hi (c0 >>= \c -> end c lo) (cN >>= \c -> end c hi))
  _ -> Nothing
  where
    readInt t = if isInteger t then Just (read (T.unpack t) :: Int) else Nothing
    inner xs = drop 1 (take (length xs - 1) xs)
    head' xs = case xs of (x : _) -> Just x; [] -> Nothing
    last' = head' . reverse

tshow :: Int -> Text
tshow = T.pack . show

-- | Every translation language used anywhere in the survey.
surveyLangs :: Form -> [Lang]
surveyLangs (Form meta items) =
  sort . nub $
    concatMap i18nLangs (maybe [] pure (metaTitle meta) ++ maybe [] pure (metaDescription meta) ++ concatMap texts (allItems items))
  where
    texts = \case
      ItemGroup g -> maybe [] pure (groupTitle g)
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q -> [qLabel q] ++ maybe [] pure (qHint q) ++ map choiceLabel (qChoices (qType q)) ++ maybe [] pure (qValidation q >>= valMessage)
    qChoices = \case
      QChoice _ _ cs -> cs
      QMatrix _ rs cs -> rs ++ cs
      QRanking cs -> cs
      _ -> []

-- | One warning for a format with no translations.
warnTranslations :: Text -> Form -> Conv ()
warnTranslations fmt s = do
  let scaleLangs = [l | q <- questions s, QScale _ _ lo hi <- [qType q], Just t <- [lo, hi], l <- i18nLangs t]
      ls = sort (nub (surveyLangs s ++ scaleLangs))
  unless (null ls) $ warn fmt ("translations dropped (" <> T.intercalate ", " ls <> ")")

-- | Items with every group dissolved, each child carrying its ancestors'
-- relevance. Groups are returned in document order for formats that can
-- list their members separately.
flattenGroups :: [Item] -> [Item]
flattenGroups = concatMap (go Nothing)
  where
    go rel = \case
      ItemGroup g -> concatMap (go (andMaybe rel (groupRelevant g))) (groupItems g)
      ItemQuestion q -> [ItemQuestion q {qRelevant = andMaybe rel (qRelevant q)}]
      ItemNote i t r -> [ItemNote i t (andMaybe rel r)]
      i -> [i]

-- | Apply a rewrite to every expression in the survey.
mapSurveyLogic :: (Expr -> Expr) -> Form -> Form
mapSurveyLogic f (Form m items) = Form m (map item items)
  where
    item = \case
      ItemGroup g -> ItemGroup g {groupRelevant = f <$> groupRelevant g, groupItems = map item (groupItems g)}
      ItemNote i t r -> ItemNote i t (f <$> r)
      ItemQuestion q ->
        ItemQuestion
          q
            { qRelevant = f <$> qRelevant q
            , qValidation = (\v -> v {valExpr = f <$> valExpr v}) <$> qValidation q
            , qType = case qType q of
                QCalculate e -> QCalculate (f e)
                t -> t
            }
      i -> i

-- | Give every question, note and group an id the format accepts: the
-- sanitiser must return valid ids unchanged, so a second pass is a no-op.
-- Empty ids get a numbered one and duplicates a suffix; references in
-- logic follow the first item of each old id. Each change is a warning.
legalIds :: Text -> (Text -> Text) -> Form -> Conv Form
legalIds fallback sanitise s@(Form m items) = do
  let olds = map itemId (filter named (allItems items))
      step (used, acc, n) old =
        let base = if T.null old then fallback <> T.pack (show n) else sanitise old
            new = head' [c | c <- base : [base <> "_" <> T.pack (show k) | k <- [2 :: Int ..]], S.notMember (T.toLower c) used]
         in (S.insert (T.toLower new) used, (old, new) : acc, n + 1)
      (_, pairsRev, _) = foldl step (S.empty, [], 1 :: Int) olds
      pairs = reverse pairsRev
      renames = M.fromListWith (\_ first -> first) pairs
  forM_ [(o, n) | (o, n) <- pairs, o /= n] $ \(o, n) -> warn (if T.null o then n else o) ("id written as " <> n)
  if all (uncurry (==)) pairs
    then pure s
    else do
      let ref q = M.findWithDefault q q renames
          fixE = mapExpr $ \case
            ERef q -> ERef (ref q)
            ESelected q v -> ESelected (ref q) v
            EAnswered q -> EAnswered (ref q)
            e -> e
          go (ItemGroup g : rest) ps = let (kids, ps') = goL (groupItems g) (drop 1 ps) in ItemGroup g {groupId = new ps, groupItems = kids} : go rest ps'
          go (ItemQuestion q : rest) ps = ItemQuestion q {qId = new ps} : go rest (drop 1 ps)
          go (ItemNote _ t r : rest) ps = ItemNote (new ps) t r : go rest (drop 1 ps)
          go (i : rest) ps = i : go rest ps
          go [] _ = []
          goL is ps = let out = go is ps in (out, drop (length (filter named (allItems is))) ps)
          new ps = maybe "" snd (listToMaybe ps)
      pure (mapSurveyLogic fixE (Form m (go items pairs)))
  where
    named = \case
      ItemRaw _ _ -> False
      _ -> True
    head' xs = fromMaybe "" (listToMaybe xs)

-- | Remove show-if conditions the dialect cannot print, with a warning,
-- so that a writer's structural decisions match what its reader will see.
dropUnprintable :: Dialect -> Form -> Conv Form
dropUnprintable d (Form m items) = Form m <$> mapM item items
  where
    keep at = \case
      Nothing -> pure Nothing
      Just e -> case printExpr d e of
        Right _ -> pure (Just e)
        Left why -> Nothing <$ warn at ("logic dropped, " <> dName d <> " cannot express " <> why)
    item = \case
      ItemGroup g -> do
        r <- keep (groupId g) (groupRelevant g)
        kids <- mapM item (groupItems g)
        pure (ItemGroup g {groupRelevant = r, groupItems = kids})
      ItemNote i t r -> ItemNote i t <$> keep i r
      ItemQuestion q -> (\r -> ItemQuestion q {qRelevant = r}) <$> keep (qId q) (qRelevant q)
      i -> pure i
