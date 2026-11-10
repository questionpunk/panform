-- | What survives a trip through each format.
--
-- Every feature the AST can express has a one-feature survey here. The
-- audit writes it in a format, reads it back, and compares: the feature
-- was kept exactly, lost with a warning, or lost silently. A silent loss
-- breaks the rule every writer follows, so the test suite fails on one.
module Panform.Audit
  ( Feature (..)
  , Outcome (..)
  , features
  , audit
  , auditMarkdown
  , normalise
  ) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Design (designAttributes)

data Feature = Feature
  { featName :: Text
  , featSurvey :: Form
  }

data Outcome
  = Kept
  | Warned [Warning]
  -- ^ Changed, and the writer or reader said so.
  | Silent
  -- ^ Changed with no warning: a bug.
  | WriteOnly [Warning]
  | Failed Text
  deriving (Eq, Show)

-- | Run one feature through a writer and (if the format has one) its reader.
audit :: Maybe (BL.ByteString -> Conv Form) -> (Form -> Conv BL.ByteString) -> Feature -> Outcome
audit mread write f = case runConv (write (featSurvey f)) of
  Left (ConvError e) -> Failed e
  Right (out, ws1) -> case mread of
    Nothing -> WriteOnly ws1
    Just r -> case runConv (r out) of
      Left (ConvError e) -> Failed ("read back: " <> e)
      Right (s, ws2)
        | normalise (dropArtefacts (featSurvey f) s) == normalise (featSurvey f) -> Kept
        | null (ws1 ++ ws2) -> Silent
        | otherwise -> Warned (ws1 ++ ws2)

-- | Structure a format must add: REDCap's record identifier, the one
-- page (instrument, block) wrapping a survey that had none, and the
-- title and language a format must name when the survey named none
-- (Qualtrics' SurveyLanguage, LimeSurvey's placeholder title).
dropArtefacts :: Form -> Form -> Form
dropArtefacts orig (Form m is) = Form m' (strip is)
  where
    om = formMeta orig
    m' =
      m
        { metaTitle = if metaTitle om == Nothing && metaTitle m == Just (plain "Survey") then Nothing else metaTitle m
        , metaLanguages = case metaLanguages m of
            [_] | null (metaLanguages om) -> []
            ls -> ls
        }
    hadGroups = not (null [() | ItemGroup _ <- formItems orig])
    strip xs = case filter (not . isRecordId) xs of
      [ItemGroup g] | not hadGroups, groupKind g == Page -> filter (not . isRecordId) (groupItems g)
      ys -> ys
    isRecordId (ItemQuestion q) = qId q == "record_id" && all ((/= "record_id") . qId) (questions orig)
    isRecordId _ = False

-- | Differences that are not information: incidental extension attributes, a single
-- untitled page wrapping everything, and empty containers.
normalise :: Form -> Form
normalise (Form m is) =
  Form m {metaAttrs = designAttributes (metaAttrs m), metaId = Nothing} (unwrap (map clean is))
  where
    unwrap [ItemGroup g] | groupKind g == Page, groupTitle g == Nothing, groupRelevant g == Nothing = groupItems g
    unwrap xs = xs
    clean = \case
      ItemGroup g -> ItemGroup g {groupAttrs = M.empty, groupItems = map clean (groupItems g)}
      ItemQuestion q -> ItemQuestion q {qAttrs = M.empty}
      i -> i

auditMarkdown :: [(String, Maybe (BL.ByteString -> Conv Form), Maybe (Form -> Conv BL.ByteString))] -> Text
auditMarkdown fmts =
  T.unlines $
    [ "| Feature | " <> T.intercalate " | " [T.pack n | (n, _, Just _) <- fmts] <> " |"
    , "|---|" <> T.concat ["---|" | (_, _, Just _) <- fmts]
    ]
      ++ [ "| " <> featName f <> " | " <> T.intercalate " | " [cell (audit r w f) | (_, r, Just w) <- fmts] <> " |"
         | f <- features
         ]
      ++ [ ""
         , "✓ kept exactly · ~ changed with a warning · ✗ changed silently (a bug) · w written with no warning (no reader) · w~ written with a warning · ! failed"
         ]
  where
    cell = \case
      Kept -> "✓"
      Warned _ -> "~"
      Silent -> "✗"
      WriteOnly [] -> "w"
      WriteOnly _ -> "w~"
      Failed _ -> "!"

------------------------------------------------------------------------------
-- The features

one :: Text -> Question -> Feature
one name q = Feature name (Form emptyMeta [ItemQuestion q])

-- | Choices whose stored value is their label, so that only the "coded
-- values" feature tests codes; formats without codes (Google Forms) would
-- otherwise warn on every feature.
same :: Text -> Choice
same t = choice t t

yn :: [Choice]
yn = [same "Yes", same "No"]

base :: Question
base = question "q1" "Do you drive?" (QChoice Single Radio yn)

follow :: Expr -> Feature -> Feature
follow e (Feature n (Form m is)) =
  Feature n (Form m (ItemQuestion base : is ++ [ItemQuestion (question "q9" "Why?" (QText LongText)) {qRelevant = Just e}]))

features :: [Feature]
features =
  [ one "single choice" base
  , one "multiple choice" (question "q1" "Which apply?" (QChoice Multiple Radio [same "Apples", same "Bananas"]))
  , one "dropdown" (question "q1" "Country" (QChoice Single Dropdown [same "United States", same "France"]))
  , one "coded values (1, 2)" (question "q1" "Pick" (QChoice Single Radio [choice "1" "Low", choice "2" "High"]))
  , one "\"other, specify\"" (question "q1" "Which apply?" (QChoice Multiple Radio [same "Apples", Choice "Other" (plain "Other") True False]))
  , one "exclusive \"none\"" (question "q1" "Which apply?" (QChoice Multiple Radio [same "Apples", Choice "None of these" (plain "None of these") False True]))
  , one "matrix (single)" (question "q1" "Rate" (QMatrix Single [same "Speed", same "Price"] [same "Bad", same "Good"]))
  , one "matrix (multiple)" (question "q1" "Tick" (QMatrix Multiple [same "Speed", same "Price"] [same "Bad", same "Good"]))
  , one "scale" (question "q1" "Recommend?" (QScale 0 10 Nothing Nothing))
  , one "scale end labels" (question "q1" "Recommend?" (QScale 0 10 (Just (plain "Never")) (Just (plain "Always"))))
  , one "ranking" (question "q1" "Order these" (QRanking [same "Apples", same "Bananas"]))
  , one "short text" (question "q1" "Name" (QText ShortText))
  , one "long text" (question "q1" "Comments" (QText LongText))
  , one "email" (question "q1" "Email" (QText Email))
  , one "integer" (question "q1" "Age" (QNumber Integer))
  , one "decimal" (question "q1" "Weight" (QNumber Decimal))
  , one "date" (question "q1" "Birthday" (QDate Date))
  , one "date and time" (question "q1" "When" (QDate DateTime))
  , one "yes/no" (question "q1" "Agree?" QBoolean)
  , one "file upload" (question "q1" "Upload" QFile)
  , one "hidden field" (question "q1" "source" QHidden)
  , one "calculation" (question "q1" "Total" (QCalculate (EBin Add (ERef "a") (ELit (LNum 1)))))
  , one "required" base {qRequired = True}
  , one "hint" base {qHint = Just (plain "Pick one")}
  , one "numeric range" (question "q1" "Age" (QNumber Integer)) {qValidation = Just (Validation (Just 18) (Just 99) Nothing Nothing)}
  , one "validation message" (question "q1" "Age" (QNumber Integer)) {qValidation = Just (Validation (Just 18) Nothing Nothing (Just (plain "Too young")))}
  , follow (EBin Eq (ERef "q1") (ELit (LStr "Yes"))) (Feature "show-if: equals" (Form emptyMeta []))
  , follow (EBin And (EBin Eq (ERef "q1") (ELit (LStr "Yes"))) (ENot (EAnswered "q9"))) (Feature "show-if: and / not" (Form emptyMeta []))
  , follow (EAnswered "q1") (Feature "show-if: answered" (Form emptyMeta []))
  , Feature "show-if: multi-select has" $
      Form
        emptyMeta
        [ ItemQuestion (question "q1" "Which apply?" (QChoice Multiple Radio [same "Apples", same "Bananas"]))
        , ItemQuestion (question "q2" "Why apples?" (QText ShortText)) {qRelevant = Just (ESelected "q1" "Apples")}
        ]
  , Feature "pages" $
      Form
        emptyMeta
        [ ItemGroup (Group "p1" Page (Just (plain "About you")) Nothing [ItemQuestion base] M.empty)
        , ItemGroup (Group "p2" Page (Just (plain "More")) Nothing [ItemQuestion (question "q2" "Name" (QText ShortText))] M.empty)
        ]
  , Feature "page show-if" $
      Form
        emptyMeta
        [ ItemGroup (Group "p1" Page (Just (plain "About you")) Nothing [ItemQuestion base] M.empty)
        , ItemGroup (Group "p2" Page (Just (plain "Drivers")) (Just (EBin Eq (ERef "q1") (ELit (LStr "Yes")))) [ItemQuestion (question "q2" "Car?" (QText ShortText))] M.empty)
        ]
  , Feature "section in a page" $
      Form
        emptyMeta
        [ItemGroup (Group "p1" Page (Just (plain "Page")) Nothing [ItemGroup (Group "s1" Section (Just (plain "Section")) Nothing [ItemQuestion base] M.empty)] M.empty)]
  , Feature "repeat / loop" $
      Form emptyMeta [ItemGroup (Group "kids" Repeat (Just (plain "Each child")) Nothing [ItemQuestion (question "q1" "Name" (QText ShortText))] M.empty)]
  , Feature "note / display text" (Form emptyMeta [ItemNote "n1" (plain "Thanks for joining.") Nothing, ItemQuestion base])
  , Feature "survey title" (Form emptyMeta {metaTitle = Just (plain "Commute study")} [ItemQuestion base])
  , Feature "survey description" (Form emptyMeta {metaTitle = Just (plain "Commute study"), metaDescription = Just (plain "Ten minutes.")} [ItemQuestion base])
  , Feature "translated labels" $
      Form
        emptyMeta {metaLanguages = ["en", "fr"]}
        [ItemQuestion base {qLabel = I18n "Do you drive?" (M.fromList [("fr", "Conduisez-vous ?")]), qType = QChoice Single Radio [Choice "Yes" (I18n "Yes" (M.fromList [("fr", "Oui")])) False False, Choice "No" (I18n "No" (M.fromList [("fr", "Non")])) False False]}]
  , visual "Jotform visual design" "jotform:design" (object ["font" .= ("Georgia" :: Text), "background" .= ("#F6F2EA" :: Text)])
  , visual "Formbricks visual design" "formbricks:design" (object ["styling" .= object ["brandColor" .= object ["light" .= ("#174A50" :: Text)]]])
  , visual "SurveyJS visual design" "surveyjs:design" (object ["logo" .= ("https://example.org/logo.png" :: Text), "widthMode" .= ("responsive" :: Text)])
  , visual "Microsoft Forms visual design" "msforms:design" (object ["otherInfo" .= ("{\"themeId\":\"custom\"}" :: Text)])
  , visual "Typeform theme reference" "typeform:theme" (object ["href" .= ("https://api.typeform.com/themes/example" :: Text)])
  ]
  where
    visual label key value = Feature label (Form emptyMeta {metaAttrs = M.singleton key value} [ItemQuestion (question "q1" "Feedback" (QText LongText))])
