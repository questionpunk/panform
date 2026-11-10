-- | Shared pieces for the variable-centric "codebook" formats (Triple-S,
-- DDI Codebook, SPSS syntax, CDISC ODM): they describe data columns rather
-- than screens, so matrices and rankings become one variable per row, and
-- a rating scale is a run of numeric codes with labels only at the ends.
module Panform.Format.Codebook
  ( Loses (..)
  , losesAll
  , flattenItems
  , lossWarnings
  , expandQuestion
  , scaleFromCodes
  , scaleCodes
  , maxScalePoints
  , scaleTooWide
  , joinI18n
  , isCode
  , readNum
  , fmtNum
  , fmtFixed
  , slug
  , tshow
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import Panform.AST
import Panform.Conv

-- | Which question properties a format has nowhere to put; each becomes a
-- warning in 'lossWarnings'.
data Loses = Loses
  { losesHint :: Bool
  , losesRequired :: Bool
  , losesLogic :: Bool
  , losesDefault :: Bool
  , losesTranslations :: Bool
  }

losesAll :: Loses
losesAll = Loses True True True True True

-- | Questions in order, with pages and sections flattened away. Anything
-- structural is warned about once under @fmt@.
flattenItems :: Text -> [Item] -> Conv [Question]
flattenItems fmt items = do
  let groups = [g | ItemGroup g <- allItems items]
  unless (null groups) $ warn fmt "pages and sections flattened (no grouping in this format)"
  forM_ groups $ \g ->
    when (isJust (groupRelevant g)) $ warn (groupId g) "group logic dropped"
  concat <$> mapM go items
  where
    go = \case
      ItemGroup g -> concat <$> mapM go (groupItems g)
      ItemQuestion q -> pure [q]
      ItemNote i _ _ -> [] <$ warn i "display text dropped"
      ItemRaw (Format f) _ -> [] <$ warn fmt ("raw " <> f <> " content dropped")

lossWarnings :: Loses -> Question -> Conv ()
lossWarnings l q = do
  let i = qId q
  when (losesHint l && isJust (qHint q)) $ warn i "hint dropped"
  when (losesRequired l && qRequired q) $ warn i "required flag dropped"
  when (losesLogic l && isJust (qRelevant q)) $ warn i "display logic dropped"
  when (losesDefault l && isJust (qDefault q)) $ warn i "default value dropped"
  when (isJust (qValidation q >>= valExpr)) $ warn i "validation expression dropped"
  when (losesTranslations l && hasTranslations q) $ warn i "translations dropped"
  where
    hasTranslations x = not (null (i18nLangs (qLabel x))) || case qType x of
      QChoice _ _ cs -> any (not . null . i18nLangs . choiceLabel) cs
      _ -> False

-- | Matrices become one question per row (@q_row@, labelled
-- "matrix: row"), rankings one integer rank per option. Other questions
-- pass through.
expandQuestion :: Question -> Conv [Question]
expandQuestion q = case qType q of
  QMatrix sel rows cols -> do
    warn (qId q) "matrix expanded into one variable per row"
    pure
      [ q {qId = qId q <> "_" <> choiceValue r, qLabel = joinI18n ": " (qLabel q) (choiceLabel r), qType = QChoice sel Radio cols}
      | r <- rows
      ]
  QRanking cs -> do
    warn (qId q) "ranking expanded into one rank variable per option"
    let n = fromIntegral (length cs)
    pure
      [ q {qId = qId q <> "_" <> choiceValue c, qLabel = joinI18n ": " (qLabel q) (choiceLabel c), qType = QNumber Integer, qValidation = Just (Validation (Just 1) (Just n) Nothing Nothing)}
      | c <- cs
      ]
  _ -> pure [q]

-- | Labels "matrix" and "row" as "matrix: row"; an empty side is omitted.
joinI18n :: Text -> I18n -> I18n -> I18n
joinI18n sep a b = I18n (j (i18nDefault a) (i18nDefault b)) (M.fromList [(l, j (i18nText (Just l) a) (i18nText (Just l) b)) | l <- langs])
  where
    langs = i18nLangs a ++ filter (`notElem` i18nLangs a) (i18nLangs b)
    j x y
      | T.null x = y
      | T.null y = x
      | otherwise = x <> sep <> y

-- | The most points of a scale that are written out one by one. A scale
-- is two numbers, so without a limit a single question could ask for
-- billions of codes.
maxScalePoints :: Int
maxScalePoints = 10000

-- | Whether a scale has more than 'maxScalePoints' points; a writer that
-- lists the points should then warn that the list stops early.
scaleTooWide :: Int -> Int -> Bool
scaleTooWide lo hi = not (null (drop maxScalePoints [lo .. hi]))

-- | Codes and labels for a scale: every point is listed (the first
-- 'maxScalePoints' of them), the ends carry the scale's labels and the
-- middle points are labelled with their number.
scaleCodes :: Int -> Int -> Maybe I18n -> Maybe I18n -> [(Text, I18n)]
scaleCodes lo hi l h = [(tshow n, lbl n) | n <- take maxScalePoints [lo .. hi]]
  where
    lbl n
      | n == lo, Just x <- l = x
      | n == hi, Just x <- h = x
      | otherwise = plain (tshow n)

-- | The inverse of 'scaleCodes': at least three contiguous integer codes
-- whose middle labels are just their numbers.
scaleFromCodes :: [(Text, I18n)] -> Maybe QType
scaleFromCodes cs = do
  ns <- mapM (readInt . fst) cs
  (lo, hi) <- case ns of
    (a : _ : _ : _) -> Just (a, last ns)
    _ -> Nothing
  let middle = drop 1 (take (length cs - 1) cs)
  if ns == [lo .. hi] && all (\(c, l) -> i18nDefault l == c && null (i18nLangs l)) middle
    then Just (QScale lo hi (endLabel (safeHead cs)) (endLabel (safeHead (reverse cs))))
    else Nothing
  where
    readInt t = case T.uncons t of
      Just ('-', r) | isCode r -> negate <$> readI r
      _ | isCode t -> readI t
      _ -> Nothing
    readI t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing
    endLabel = \case
      Just (c, l) | i18nDefault l /= c || not (null (i18nLangs l)) -> Just l
      _ -> Nothing
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing

-- | A non-negative integer code such as @3@ or @01@.
isCode :: Text -> Bool
isCode t = not (T.null t) && T.all isDigit t

readNum :: Text -> Maybe Double
readNum t0 = case reads (T.unpack (fixLeading (T.strip t0))) of
  [(d, "")] -> Just d
  _ -> Nothing
  where
    fixLeading t = case T.uncons t of
      Just ('.', _) -> "0" <> t
      Just ('-', r) | "." `T.isPrefixOf` r -> "-0" <> r
      _ -> t

-- | A number without a trailing @.0@ when it is whole.
fmtNum :: Double -> Text
fmtNum x
  | x == fromIntegral r = tshow r
  | otherwise = T.pack (showFFloat Nothing x "")
  where
    r = round x :: Integer

-- | A number with a fixed count of decimals.
fmtFixed :: Int -> Double -> Text
fmtFixed d x = T.pack (showFFloat (Just d) x "")

slug :: Text -> Text
slug t =
  let s = T.intercalate "_" (T.words (T.map (\c -> if isAlphaNum c then c else ' ') (T.toLower t)))
   in if T.null s then "survey" else s

tshow :: Show a => a -> Text
tshow = T.pack . show
