{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | The intermediate representation every reader produces and every writer
-- consumes. Like pandoc's 'Pandoc' type, it is the union of what the
-- supported formats can express; a feature a target cannot represent is
-- dropped by the writer with a 'Panform.Conv.Warning', never silently.
module Panform.AST
  ( Form (..)
  , Meta (..)
  , emptyMeta
  , Lang
  , I18n (..)
  , plain
  , i18nText
  , i18nLangs
  , Item (..)
  , GroupKind (..)
  , Group (..)
  , Question (..)
  , question
  , QType (..)
  , TextKind (..)
  , NumKind (..)
  , DateKind (..)
  , Selection (..)
  , ChoiceStyle (..)
  , Choice (..)
  , choice
  , Validation (..)
  , Expr (..)
  , Lit (..)
  , BinOp (..)
  , Format (..)
  , Attrs
  , questions
  , allItems
  , itemId
  , usedLanguages
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A language tag such as @en@, @fr@ or @pt-BR@.
type Lang = Text

-- | Free-form extension data that survives a round trip through the native
-- format, pandoc-attribute style. Readers put anything the IR has no field
-- for here, keyed by @format:key@.
type Attrs = Map Text Value

data Form = Form
  { formMeta :: Meta
  , formItems :: [Item]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Meta = Meta
  { metaId :: Maybe Text
  , metaTitle :: Maybe I18n
  , metaDescription :: Maybe I18n
  , metaLanguages :: [Lang]
  -- ^ First entry is the default language. Empty means "unspecified".
  , metaAttrs :: Attrs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta Nothing Nothing Nothing [] M.empty

-- | Translatable text. 'i18nDefault' is the text in the default language;
-- 'i18nTranslations' holds the others.
data I18n = I18n
  { i18nDefault :: Text
  , i18nTranslations :: Map Lang Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

plain :: Text -> I18n
plain t = I18n t M.empty

-- | Text in a given language, falling back to the default.
i18nText :: Maybe Lang -> I18n -> Text
i18nText Nothing t = i18nDefault t
i18nText (Just l) t = M.findWithDefault (i18nDefault t) l (i18nTranslations t)

i18nLangs :: I18n -> [Lang]
i18nLangs = M.keys . i18nTranslations

data Item
  = ItemGroup Group
  | ItemQuestion Question
  | ItemNote Text I18n (Maybe Expr)
  -- ^ Display-only text (id, text, relevance).
  | ItemRaw Format Value
  -- ^ Format-specific content passed through untouched, like pandoc's
  -- @RawBlock@. Writers emit it only when the format matches.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GroupKind
  = Page
  -- ^ A screen break (SurveyJS page, Qualtrics block, LimeSurvey group).
  | Section
  -- ^ A visual grouping on the same page (XLSForm field-list group).
  | Repeat
  -- ^ A roster/loop asked repeatedly (XLSForm repeat, REDCap repeating form).
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Group = Group
  { groupId :: Text
  , groupKind :: GroupKind
  , groupTitle :: Maybe I18n
  , groupRelevant :: Maybe Expr
  , groupItems :: [Item]
  , groupAttrs :: Attrs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question = Question
  { qId :: Text
  -- ^ Variable name; the join key for logic and response data.
  , qLabel :: I18n
  , qHint :: Maybe I18n
  , qType :: QType
  , qRequired :: Bool
  , qRelevant :: Maybe Expr
  -- ^ Display/skip condition: ask only when true.
  , qValidation :: Maybe Validation
  , qDefault :: Maybe Text
  , qAttrs :: Attrs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

question :: Text -> Text -> QType -> Question
question i l t = Question i (plain l) Nothing t False Nothing Nothing Nothing M.empty

data TextKind = ShortText | LongText | Email | Url | Phone
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data NumKind = Integer | Decimal
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data DateKind = Date | Time | DateTime
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data Selection = Single | Multiple
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data ChoiceStyle = Radio | Dropdown | Buttons
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data Choice = Choice
  { choiceValue :: Text
  -- ^ Coded value stored in response data.
  , choiceLabel :: I18n
  , choiceOther :: Bool
  -- ^ "Other, please specify" with a free-text box.
  , choiceExclusive :: Bool
  -- ^ Clears other selections ("None of the above").
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

choice :: Text -> Text -> Choice
choice v l = Choice v (plain l) False False

data QType
  = QText TextKind
  | QNumber NumKind
  | QChoice Selection ChoiceStyle [Choice]
  | QMatrix Selection [Choice] [Choice]
  -- ^ Rows (sub-questions), then columns (answer scale).
  | QScale Int Int (Maybe I18n) (Maybe I18n)
  -- ^ Linear scale / rating: min, max, low label, high label.
  | QRanking [Choice]
  | QDate DateKind
  | QBoolean
  | QFile
  | QGeo
  | QCalculate Expr
  -- ^ Hidden computed value.
  | QHidden
  -- ^ Embedded data / URL parameter carried with the response.
  | QOther Text
  -- ^ A type the IR does not model, kept by name.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Validation = Validation
  { valMin :: Maybe Double
  , valMax :: Maybe Double
  , valExpr :: Maybe Expr
  -- ^ Must evaluate true; @Ref "."@ is the current answer.
  , valMessage :: Maybe I18n
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A dialect-neutral logic expression. Each format has a printer and a
-- parser for its own syntax in "Panform.Expr".
data Expr
  = ELit Lit
  | ERef Text
  -- ^ A question's answer.
  | ESelected Text Text
  -- ^ Multi-select question contains the choice value.
  | EAnswered Text
  -- ^ Question has a non-empty answer.
  | ENot Expr
  | EBin BinOp Expr Expr
  | ECall Text [Expr]
  -- ^ Function the IR does not model; printed verbatim by name.
  | ERaw Text Text
  -- ^ Unparsed source in the named dialect.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Lit = LStr Text | LNum Double | LBool Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BinOp = And | Or | Eq | Neq | Lt | Le | Gt | Ge | Add | Sub | Mul | Div
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

newtype Format = Format Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Every question, depth first.
questions :: Form -> [Question]
questions s = [q | ItemQuestion q <- allItems (formItems s)]

-- | Every item, depth first, groups before their children.
allItems :: [Item] -> [Item]
allItems = concatMap go
  where
    go i@(ItemGroup g) = i : allItems (groupItems g)
    go i = [i]

itemId :: Item -> Text
itemId = \case
  ItemGroup g -> groupId g
  ItemQuestion q -> qId q
  ItemNote i _ _ -> i
  ItemRaw (Format f) _ -> f

-- | Every translation language used anywhere in the survey, in first-use order.
usedLanguages :: Form -> [Lang]
usedLanguages (Form m is) =
  nub (concatMap i18nLangs (maybe [] pure (metaTitle m) ++ maybe [] pure (metaDescription m) ++ concatMap texts (allItems is)))
  where
    texts = \case
      ItemGroup g -> maybe [] pure (groupTitle g)
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q ->
        [qLabel q] ++ maybe [] pure (qHint q) ++ maybe [] pure (qValidation q >>= valMessage) ++ case qType q of
          QChoice _ _ cs -> map choiceLabel cs
          QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
          QRanking cs -> map choiceLabel cs
          QScale _ _ l h -> maybe [] pure l ++ maybe [] pure h
          _ -> []
