-- | Helpers shared by the plain-text writers (GIFT, Aiken, Markdown, HTML,
-- surveydown): formats with one language, flat pages and no coded
-- matrices, so the same flattening and loss warnings recur.
module Panform.Format.TextUtil
  ( txt
  , warnTranslations
  , paginate
  , pageId
  , flattenGroups
  , scaleChoices
  , matrixAsQuestions
  , surveyTexts
  , showNumber
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import Panform.AST
import Panform.Conv
import Panform.Format.Codebook (maxScalePoints)

-- | Default-language text.
txt :: I18n -> Text
txt = i18nDefault

-- | Warn once when the survey carries translations a single-language
-- format cannot hold.
warnTranslations :: Text -> Form -> Conv ()
warnTranslations fmt s =
  case [l | t <- surveyTexts s, l <- i18nLangs t] of
    [] -> pure ()
    ls -> warn fmt ("translations dropped (" <> T.intercalate ", " (dedupe ls) <> "); default language only")
  where
    dedupe = M.keys . M.fromList . map (\l -> (l, ()))

-- | Every translatable text in the survey.
surveyTexts :: Form -> [I18n]
surveyTexts (Form m items) =
  maybeToList (metaTitle m) ++ maybeToList (metaDescription m) ++ concatMap itemTexts (allItems items)
  where
    itemTexts = \case
      ItemGroup g -> maybeToList (groupTitle g)
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q ->
        qLabel q : maybeToList (qHint q) ++ maybeToList (qValidation q >>= valMessage) ++ typeTexts (qType q)
    typeTexts = \case
      QChoice _ _ cs -> map choiceLabel cs
      QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
      QScale _ _ l h -> maybeToList l ++ maybeToList h
      QRanking cs -> map choiceLabel cs
      _ -> []

-- | Top-level pages as they are; loose items between pages become a page.
paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

-- | A page's id, or a positional one for a synthesized page.
pageId :: Int -> Maybe Group -> Text
pageId n = maybe ("page" <> T.pack (show n)) groupId

-- | Splice nested groups into their parent for formats with no grouping
-- below the page; the group's title and condition are lost.
flattenGroups :: [Item] -> Conv [Item]
flattenGroups = fmap concat . mapM go
  where
    go = \case
      ItemGroup g -> do
        warn (groupId g) "nested group flattened into its page"
        mapM_ (const (warn (groupId g) "group condition dropped")) (groupRelevant g)
        flattenGroups (groupItems g)
      i -> pure [i]

-- | A linear scale as numbered choices; the end labels go on the ends.
-- At most 'maxScalePoints' points are listed ('scaleTooWide' says when
-- that cuts a scale short).
scaleChoices :: Int -> Int -> Maybe I18n -> Maybe I18n -> [Choice]
scaleChoices lo hi l h =
  [ choice v (v <> endLabel n)
  | n <- take maxScalePoints [lo .. hi]
  , let v = T.pack (show n)
  ]
  where
    endLabel n
      | n == lo, Just t <- l = " (" <> txt t <> ")"
      | n == hi, Just t <- h = " (" <> txt t <> ")"
      | otherwise = ""

-- | A matrix as one single/multiple choice question per row, ids
-- @matrix_row@.
matrixAsQuestions :: Question -> Selection -> [Choice] -> [Choice] -> [Question]
matrixAsQuestions q sel rows cols =
  [ q
      { qId = qId q <> "_" <> choiceValue r
      , qLabel = plain (txt (qLabel q) <> " - " <> txt (choiceLabel r))
      , qType = QChoice sel Radio cols
      }
  | r <- rows
  ]

-- | A number without a trailing @.0@.
showNumber :: Double -> Text
showNumber n
  | n == fromIntegral r = T.pack (show r)
  | otherwise = T.pack (showFFloat Nothing n "")
  where
    r = round n :: Integer
