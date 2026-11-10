-- | The LimeSurvey survey model shared by the @.lss@ XML and the
-- tab-separated text readers and writers: groups (pages) of questions,
-- each with a one-letter type code, sub-questions (the items of a multiple
-- choice, the rows and columns of an array) and answers (the options), all
-- texts per language, and ExpressionScript relevance.
module Panform.Format.LimeSurvey
  ( LsSurvey (..)
  , LsGroup (..)
  , LsQuestion (..)
  , LsSub (..)
  , LsAnswer (..)
  , LsCond (..)
  , noText
  , mergeI18n
  , i18nFrom
  , lsQuestion
  , fromLs
  , toLs
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON, Result (..), Value (..), fromJSON, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlpha, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find, groupBy, mapAccumL, nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Expr

data LsSurvey = LsSurvey
  { lsId :: Text
  -- ^ Survey id (@sid@), empty when unknown.
  , lsLangs :: [Lang]
  -- ^ Base language first.
  , lsSettings :: [(Text, Text)]
  -- ^ Survey-level settings other than sid and languages.
  , lsLangSettings :: [(Text, I18n)]
  -- ^ Per-language settings (@surveyls_title@, ...).
  , lsGroups :: [LsGroup]
  }
  deriving (Eq, Show)

data LsGroup = LsGroup
  { lgId :: Text
  , lgName :: I18n
  , lgDesc :: I18n
  , lgRelevance :: Text
  , lgRandom :: Text
  , lgQuestions :: [LsQuestion]
  }
  deriving (Eq, Show)

data LsQuestion = LsQuestion
  { lqId :: Text
  , lqType :: Text
  , lqTitle :: Text
  , lqText :: I18n
  , lqHelp :: I18n
  , lqRelevance :: Text
  , lqPreg :: Text
  , lqMandatory :: Text
  , lqOther :: Text
  , lqTheme :: Text
  , lqDefault :: Maybe I18n
  , lqFields :: [(Text, Text)]
  -- ^ Other question columns (encrypted, same_default, ...), defaults omitted.
  , lqAttrs :: [(Text, I18n)]
  -- ^ Advanced question attributes, sorted by name.
  , lqSubs :: [LsSub]
  , lqAnswers :: [LsAnswer]
  , lqConds :: [LsCond]
  }
  deriving (Eq, Show)

data LsSub = LsSub
  { sqId :: Text
  , sqCode :: Text
  , sqScale :: Int
  , sqText :: I18n
  , sqRelevance :: Text
  , sqDefault :: Maybe I18n
  }
  deriving (Eq, Show)

data LsAnswer = LsAnswer
  { aCode :: Text
  , aScale :: Int
  , aText :: I18n
  , aAssess :: Text
  }
  deriving (Eq, Show)

-- | A legacy (pre-ExpressionScript) condition row.
data LsCond = LsCond
  { cScenario :: Text
  , cQid :: Text
  , cField :: Text
  , cMethod :: Text
  , cValue :: Text
  }
  deriving (Eq, Show)

noText :: I18n
noText = plain ""

-- | Texts by language into an 'I18n' with the base language as default.
-- Translations identical to the default add nothing and are left out.
i18nFrom :: Lang -> [(Lang, Text)] -> I18n
i18nFrom base vs =
  let d = fromMaybe (maybe "" snd (safeHead vs)) (lookup base vs)
   in I18n d (M.fromList [(l, t) | (l, t) <- vs, l /= base, not (T.null t), t /= d])

-- | Add a language's text to an 'I18n'.
mergeI18n :: Lang -> Text -> I18n -> I18n
mergeI18n l t (I18n d tr)
  | T.null t || t == d = I18n d tr
  | otherwise = I18n d (M.insert l t tr)

-- | A question with nothing set beyond its type and code.
lsQuestion :: Text -> Text -> LsQuestion
lsQuestion ty title = LsQuestion "" ty title noText noText "1" "" "N" "N" "" Nothing [] [] [] [] []

safeHead :: [a] -> Maybe a
safeHead = \case
  (x : _) -> Just x
  [] -> Nothing

------------------------------------------------------------------------------
-- LimeSurvey -> IR

-- | Main question ids to codes, for resolving @SIDXGIDXQID@ field names.
type Codes = M.Map Text Text

fromLs :: LsSurvey -> Conv Form
fromLs ls = do
  let codes = M.fromList [(lqId q, lqTitle q) | g <- lsGroups ls, q <- lgQuestions g, not (T.null (lqId q))]
  groups <- forM (lsGroups ls) $ \g -> do
    items <- mapM (readQuestion codes) (lgQuestions g)
    pure . ItemGroup $
      Group
        { groupId = "G" <> lgId g
        , groupKind = Page
        , groupTitle = nonEmptyI18n (lgName g)
        , groupRelevant = relevance codes (lgRelevance g)
        , groupItems = items
        , groupAttrs =
            M.fromList $
              [("limesurvey:description", toJSON (lgDesc g)) | not (emptyI18n (lgDesc g))]
                ++ [("limesurvey:randomization_group", String (lgRandom g)) | not (T.null (lgRandom g))]
        }
  let langSet = lsLangSettings ls
      extra = [(k, v) | (k, v) <- langSet, k `notElem` ["surveyls_title", "surveyls_description"], not (emptyI18n v)]
      meta =
        Meta
          { metaId = if T.null (lsId ls) then Nothing else Just (lsId ls)
          , metaTitle = lookup "surveyls_title" langSet >>= nonEmptyI18n
          , metaDescription = lookup "surveyls_description" langSet >>= nonEmptyI18n
          , metaLanguages = lsLangs ls
          , metaAttrs =
              M.fromList $
                [("limesurvey:settings", object' [(k, String v) | (k, v) <- lsSettings ls]) | not (null (lsSettings ls))]
                  ++ [("limesurvey:language_settings", object' [(k, toJSON v) | (k, v) <- extra]) | not (null extra)]
          }
  pure (Form meta groups)

object' :: [(Text, Value)] -> Value
object' kvs = Object (KM.fromList [(K.fromText k, v) | (k, v) <- kvs])

emptyI18n :: I18n -> Bool
emptyI18n (I18n d tr) = T.null d && M.null tr

nonEmptyI18n :: I18n -> Maybe I18n
nonEmptyI18n t = if emptyI18n t then Nothing else Just t

-- | Relevance equation; @1@ or blank means always shown.
relevance :: Codes -> Text -> Maybe Expr
relevance codes t = case T.strip t of
  s | s `elem` ["", "1"] -> Nothing
  s -> Just (parseOrRaw limesurvey (resolveSgqa codes s))

-- | Replace @SIDXGIDXQID@-style field names, which old surveys and
-- condition-generated relevance use, with question codes.
resolveSgqa :: Codes -> Text -> Text
resolveSgqa codes = T.pack . go . T.unpack
  where
    isWord c = isAlphaNum c || c == '_'
    fix w = fromMaybe w (sgqaName codes w)
    -- A quoted string is an answer, never a field name.
    go = \case
      [] -> []
      s@(c : rest)
        | c == '"' || c == '\'' -> let (lit, after) = literal c rest in c : lit ++ go after
        | isWord c -> let (w, after) = span isWord s in T.unpack (fix (T.pack w)) ++ go after
        | otherwise -> c : go rest
    literal q = \case
      [] -> ([], [])
      ('\\' : x : xs) -> let (l, after) = literal q xs in ('\\' : x : l, after)
      (x : xs)
        | x == q -> ([x], xs)
        | otherwise -> let (l, after) = literal q xs in (x : l, after)

sgqaName :: Codes -> Text -> Maybe Text
sgqaName codes w = case T.splitOn "X" w of
  (s : g : rest@(_ : _))
    | isNum s
    , isNum g ->
        let tailPart = T.intercalate "X" rest
            digits = T.takeWhile isDigit tailPart
            cands = [(p, T.drop (T.length p) tailPart) | n <- reverse [1 .. T.length digits], let p = T.take n digits, M.member p codes]
         in case cands of
              ((qid, suffix) : _) -> (\c -> if T.null suffix then c else c <> "_" <> suffix) <$> M.lookup qid codes
              [] -> Nothing
  _ -> Nothing
  where
    isNum t = not (T.null t) && T.all isDigit t

-- | Legacy conditions: rows of one scenario on the same field are or-ed,
-- different fields and-ed, and scenarios or-ed.
conditions :: Codes -> Text -> [LsCond] -> Conv (Maybe Expr)
conditions codes at cs = do
  scenarios <- forM (groupOn cScenario (sortOn cScenario cs)) $ \sc -> do
    fields <- forM (byField sc) $ \fcs -> catMaybes <$> mapM atom fcs
    pure (joinWith "and" (mapMaybe (joinWith "or") fields))
  pure (parseOrRaw limesurvey <$> joinWith "or" (catMaybes scenarios))
  where
    groupOn f = groupBy ((==) `on` f)
    -- Rows on one field, in the order the fields first appear; the rows
    -- of a field need not be next to each other.
    byField sc =
      let firstSeen = M.fromListWith (\_ old -> old) (zip (map cField sc) [0 :: Int ..])
       in map reverse (M.elems (M.fromListWith (++) [(M.findWithDefault 0 (cField c) firstSeen, [c]) | c <- sc]))
    joinWith op xs = case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> Just ("(" <> T.intercalate (") " <> op <> " (") xs <> ")")
    atom c = do
      let (multi, field) = case T.uncons (cField c) of
            Just ('+', f) -> (True, f)
            _ -> (False, cField c)
          value = cValue c
      case sgqaName codes field of
        Nothing -> Nothing <$ warn at ("condition on unknown field " <> cField c <> " dropped")
        Just name -> case (T.stripPrefix "@" value >>= T.stripSuffix "@", cMethod c) of
          _ | "{" `T.isPrefixOf` value -> Nothing <$ warn at ("condition on " <> value <> " dropped")
          (Just other, m) -> case sgqaName codes other of
            Just o | m /= "RX" -> pure (Just (name <> ".NAOK " <> m <> " " <> o <> ".NAOK"))
            _ -> Nothing <$ warn at ("condition comparing with " <> value <> " dropped")
          (Nothing, "RX") -> pure (Just ("regexMatch(" <> quote value <> ", " <> name <> ".NAOK)"))
          (Nothing, m)
            | multi -> pure (Just (name <> "_" <> value <> ".NAOK " <> m <> " \"Y\""))
            | m `elem` ["<", "<=", ">", ">="], isNumber value -> pure (Just (name <> ".NAOK " <> m <> " " <> value))
            | m `elem` ["==", "!=", "<", "<=", ">", ">="] -> pure (Just (name <> ".NAOK " <> m <> " " <> quote value))
            | otherwise -> Nothing <$ warn at ("condition method " <> m <> " dropped")
    quote v = "\"" <> T.replace "\"" "\\\"" v <> "\""
    isNumber v = case reads (T.unpack v) :: [(Double, String)] of
      [(_, "")] -> True
      _ -> False

-- | Attribute names the reader turns into IR fields rather than keeping.
derived :: Text -> [Text]
derived ty =
  ["other_replace_text", "em_validation_q", "em_validation_q_tip"]
    ++ ["exclude_all_others" | ty `elem` ["M", "P"]]
    ++ (if ty == "N" then ["min_num_value_n", "max_num_value_n", "num_value_int_only"] else [])
    ++ ["equation" | ty == "*"]
    ++ ["multiflexible_checkbox" | ty == ":"]

readQuestion :: Codes -> LsQuestion -> Conv Item
readQuestion codes q = do
  rel <- case relevance codes (lqRelevance q) of
    Nothing | not (null (lqConds q)) -> conditions codes title (lqConds q)
    r -> pure r
  let hidden = attrText "hidden" == "1" && ty == "S"
      kept = [(k, v) | (k, v) <- lqAttrs q, k `notElem` derived ty, not (hidden && k == "hidden")]
      attrs =
        M.fromList $
          [("limesurvey:type", String ty)]
            ++ [("limesurvey:attributes", object' [(k, toJSON v) | (k, v) <- kept]) | not (null kept)]
            ++ [("limesurvey:fields", object' [(k, String v) | (k, v) <- lqFields q]) | not (null (lqFields q))]
            ++ [("limesurvey:preg", String (lqPreg q)) | not (T.null (lqPreg q))]
            ++ [("limesurvey:mandatory", String "S") | lqMandatory q == "S"]
            ++ [("limesurvey:theme", String (lqTheme q)) | not (T.null (lqTheme q))]
            ++ [("limesurvey:scale1", toJSON (answersOn 1)) | ty == "1"]
            ++ [("limesurvey:assessment_values", object' assessments) | not (null assessments)]
      assessments = [(aCode a, String (aAssess a)) | a <- lqAnswers q, aAssess a `notElem` ["", "0"]]
      base qt =
        Question
          { qId = title
          , qLabel = lqText q
          , qHint = nonEmptyI18n (lqHelp q)
          , qType = qt
          , qRequired = lqMandatory q `elem` ["Y", "S"]
          , qRelevant = rel
          , qValidation = validation
          , qDefault = i18nDefault <$> lqDefault q
          , qAttrs = attrs
          }
      one qt = pure (ItemQuestion (base qt))
  unless (null [s | s <- lqSubs q, isJust (sqDefault s)]) $ warn title "sub-question default values dropped"
  unless (ty `elem` ["Q", "K"] || all (isNothing . relevance codes . sqRelevance) (lqSubs q)) $
    warn title "sub-question relevance dropped"
  case ty of
    "L" -> one (QChoice Single Radio (answersOn 0 ++ other))
    "!" -> one (QChoice Single Dropdown (answersOn 0 ++ other))
    "O" -> do
      warn title "list with comment: the comment box is kept only as limesurvey:type"
      one (QChoice Single Radio (answersOn 0 ++ other))
    "M" -> one (QChoice Multiple Radio (subsOn 0 ++ other))
    "P" -> do
      warn title "multiple choice with comments: comment boxes kept only as limesurvey:type"
      one (QChoice Multiple Radio (subsOn 0 ++ other))
    "G" -> one (QChoice Single Radio [choice "F" "Female", choice "M" "Male"])
    "5" -> one (QScale 1 5 Nothing Nothing)
    "Y" -> one QBoolean
    "F" -> one (QMatrix Single (rows 0) (answersOn 0))
    "H" -> one (QMatrix Single (rows 0) (answersOn 0))
    "A" -> one (QMatrix Single (rows 0) (fixedColumns "A"))
    "B" -> one (QMatrix Single (rows 0) (fixedColumns "B"))
    "C" -> one (QMatrix Single (rows 0) (fixedColumns "C"))
    "E" -> one (QMatrix Single (rows 0) (fixedColumns "E"))
    "1" -> do
      warn title "dual-scale array: the second scale is kept only as limesurvey:scale1"
      one (QMatrix Single (rows 0) (answersOn 0))
    ":"
      | attrText "multiflexible_checkbox" == "1" -> one (QMatrix Multiple (rows 0) (rows 1))
      | otherwise -> do
          warn title "array of numbers read as a single-choice grid of its columns"
          one (QMatrix Single (rows 0) (rows 1))
    ";" -> do
      warn title "array of texts read as a single-choice grid of its columns"
      one (QMatrix Single (rows 0) (rows 1))
    "S" -> one (if hidden then QHidden else QText ShortText)
    "T" -> one (QText LongText)
    "U" -> one (QText LongText)
    "N" -> one (QNumber (if attrText "num_value_int_only" == "1" then Integer else Decimal))
    "D" -> one (QDate (dateKind (attrText "date_format")))
    "R" -> one (QRanking (answersOn 0))
    "|" -> one QFile
    "*" ->
      let src = if T.null (T.strip (attrText "equation")) then i18nDefault (lqText q) else attrText "equation"
       in one (QCalculate (parseOrRaw limesurvey (resolveSgqa codes (equation src))))
    "X" -> pure (ItemNote title (lqText q) rel)
    "I" -> do
      warn title "language switch kept as an untyped question"
      one (QOther "languageswitch")
    "Q" -> pure (multiText (QText ShortText) rel attrs)
    "K" -> pure (multiText (QNumber Decimal) rel attrs)
    other -> do
      warn title ("unsupported question type " <> other <> " kept by name")
      one (QOther other)
  where
    ty = lqType q
    title = lqTitle q
    attrI18n k = lookup k (lqAttrs q)
    attrText k = maybe "" i18nDefault (attrI18n k)
    answersOn n = [Choice (aCode a) (aText a) False False | a <- lqAnswers q, aScale a == n]
    -- Codes are separated by ";" (older surveys also used spaces or commas).
    exclusive = let v = attrText "exclude_all_others" in map T.strip (T.splitOn ";" v) ++ T.split (\c -> c == ' ' || c == ',') v
    subsOn n = [Choice (sqCode s) (sqText s) False (sqCode s `elem` exclusive) | s <- lqSubs q, sqScale s == n]
    rows n = [Choice (sqCode s) (sqText s) False False | s <- lqSubs q, sqScale s == n]
    other = [Choice "other" (fromMaybe (plain "Other") (attrI18n "other_replace_text" >>= nonEmptyI18n)) True False | lqOther q == "Y"]
    validation =
      let num k = if ty == "N" then readNum (attrText k) else Nothing
          ex = parseOrRaw limesurvey . resolveSgqa codes <$> nonBlank (attrText "em_validation_q")
          v = Validation (num "min_num_value_n") (num "max_num_value_n") (selfRef title <$> ex) (attrI18n "em_validation_q_tip" >>= nonEmptyI18n)
       in if v == Validation Nothing Nothing Nothing Nothing then Nothing else Just v
    -- Multiple short texts and multiple numerics are a section of plain
    -- questions named @code_sub@, which is how ExpressionScript names them.
    multiText qt rel attrs =
      ItemGroup
        Group
          { groupId = title
          , groupKind = Section
          , groupTitle = Just (lqText q)
          , groupRelevant = rel
          , groupItems =
              [ ItemQuestion
                  (question (title <> "_" <> sqCode s) "" qt)
                    { qLabel = sqText s
                    , qRequired = lqMandatory q `elem` ["Y", "S"]
                    , qRelevant = relevance codes (sqRelevance s)
                    }
              | s <- lqSubs q
              ]
          , groupAttrs = M.union attrs (M.fromList [("limesurvey:help", toJSON (lqHelp q)) | not (emptyI18n (lqHelp q))])
          }

-- | The self reference in a validation equation (@this@ or the question's
-- own code) as the IR's @.@.
selfRef :: Text -> Expr -> Expr
selfRef self = go
  where
    go e = case e of
      ERef r | r == self || r == "this" -> ERef "."
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      _ -> e

-- | The expression of an equation: the inside of its one @{...}@ block
-- (older surveys put it in the question text, among other words).
equation :: Text -> Text
equation t = case T.splitOn "{" t of
  [_, rest] | [inner, _] <- T.splitOn "}" rest -> T.strip inner
  _ -> T.strip t

dateKind :: Text -> DateKind
dateKind f
  | hasTime && hasDate = DateTime
  | hasTime = Time
  | otherwise = Date
  where
    hasTime = "HH" `T.isInfixOf` f || "H:" `T.isInfixOf` f
    hasDate = any (`T.isInfixOf` T.toLower f) ["yy", "dd", "mm/"]

-- | The answers LimeSurvey builds into its fixed-scale array types.
fixedColumns :: Text -> [Choice]
fixedColumns = \case
  "A" -> [choice (tshow n) (tshow n) | n <- [1 .. 5 :: Int]]
  "B" -> [choice (tshow n) (tshow n) | n <- [1 .. 10 :: Int]]
  "C" -> [choice "Y" "Yes", choice "N" "No", choice "U" "Uncertain"]
  "E" -> [choice "I" "Increase", choice "S" "Same", choice "D" "Decrease"]
  _ -> []

------------------------------------------------------------------------------
-- IR -> LimeSurvey

toLs :: Form -> Conv LsSurvey
toLs (Form meta items) = do
  let base = fromMaybe "en" (safeHead (metaLanguages meta))
      langs = base : filter (/= base) (nub (drop 1 (metaLanguages meta) ++ surveyLangs meta items))
      stored k = M.lookup k (metaAttrs meta)
      settings = [(k, v) | Just (Object o) <- [stored "limesurvey:settings"], (k', String v) <- KM.toList o, let k = K.toText k', k `notElem` ["sid", "language", "additional_languages"]]
      langSettings = [(K.toText k, v) | Just (Object o) <- [stored "limesurvey:language_settings"], (k, j) <- KM.toList o, Just v <- [decode j]]
      rename = renames items
  forM_ (M.toList rename) $ \(old, new) ->
    warn old ("renamed to " <> new <> "; LimeSurvey codes are a letter then letters or digits")
  let pages = paginate items
      gids = groupNumbers [g >>= T.stripPrefix "G" . groupId | (g, _) <- pages]
  groups <- forM (zip3 [1 :: Int ..] gids pages) $ \(n, gid, (g, kids)) -> do
    forM_ g $ \gr ->
      if isGid (groupId gr)
        then unless (groupId gr == "G" <> gid) $ warn (groupId gr) ("group number used twice; renumbered " <> gid)
        else warn (groupId gr) "page id dropped; LimeSurvey numbers its groups"
    rel <- printLogic limesurvey ("group " <> tshow n) (renameExpr rename <$> (g >>= groupRelevant))
    qs <- concat <$> mapM (itemQuestions rename) kids
    pure
      LsGroup
        { lgId = gid
        , lgName = maybe (plain ("Group " <> tshow n)) (\gr -> fromMaybe (plain (groupId gr)) (groupTitle gr >>= nonEmptyI18n)) g
        , lgDesc = fromMaybe noText (g >>= M.lookup "limesurvey:description" . groupAttrs >>= decode)
        , lgRelevance = fromMaybe "1" rel
        , lgRandom = fromMaybe "" (g >>= textAttr "limesurvey:randomization_group" . groupAttrs)
        , lgQuestions = qs
        }
  let titles = [lqTitle q | g <- groups, q <- lgQuestions g]
  forM_ (nub [t | (i, t) <- zip [0 :: Int ..] titles, t `elem` take i titles]) $ \t ->
    warn t "question code is used twice; LimeSurvey will reject it"
  pure
    LsSurvey
      { lsId = maybe "" (\i -> if T.all isDigit i && not (T.null i) then i else "") (metaId meta)
      , lsLangs = langs
      , lsSettings = settings
      , lsLangSettings =
          [("surveyls_title", fromMaybe (plain "Survey") (metaTitle meta))]
            ++ [("surveyls_description", d) | Just d <- [metaDescription meta]]
            ++ [(k, v) | (k, v) <- langSettings, k `notElem` ["surveyls_title", "surveyls_description"]]
      , lsGroups = numberIds groups
      }

-- | Group numbers: a group this reader made keeps its number; any other
-- group gets the next free one.
groupNumbers :: [Maybe Text] -> [Text]
groupNumbers ns = snd (mapAccumL pick (1 :: Int) (zip [0 :: Int ..] ns))
  where
    -- The first group to claim a number keeps it.
    firsts = [(i, n) | (i, Just n) <- zip [0 ..] ns, isGid ("G" <> n), Just n `notElem` take i ns]
    kept = map snd firsts
    pick next (i, _) = case lookup i firsts of
      Just n -> (next, n)
      Nothing -> let k = fromMaybe next (find (\k' -> tshow k' `notElem` kept) [next ..]) in (k + 1, tshow k)

-- | A group id this reader made (@G@ and the group number), which carries
-- nothing a renumbering loses.
isGid :: Text -> Bool
isGid t = case T.stripPrefix "G" t of
  Just n -> not (T.null n) && T.all isDigit n
  Nothing -> False

decode :: FromJSON a => Value -> Maybe a
decode v = case fromJSON v of
  Success a -> Just a
  Error _ -> Nothing

textAttr :: Text -> Attrs -> Maybe Text
textAttr k as = case M.lookup k as of
  Just (String t) -> Just t
  _ -> Nothing

-- | Every language any text is translated into.
surveyLangs :: Meta -> [Item] -> [Lang]
surveyLangs meta items = nub (concatMap i18nLangs (catMaybes [metaTitle meta, metaDescription meta]) ++ concatMap itemLangs (allItems items))
  where
    itemLangs = \case
      ItemQuestion q -> concatMap i18nLangs ([qLabel q] ++ catMaybes [qHint q] ++ concatMap choiceTexts [qType q])
      ItemGroup g -> maybe [] i18nLangs (groupTitle g)
      ItemNote _ t _ -> i18nLangs t
      ItemRaw _ _ -> []
    choiceTexts = \case
      QChoice _ _ cs -> map choiceLabel cs
      QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
      QRanking cs -> map choiceLabel cs
      QScale _ _ l h -> catMaybes [l, h]
      _ -> []

-- | Top-level pages as they are; loose items between pages become a group.
paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

-- | Question codes must start with a letter and hold only letters and
-- digits; anything else is renamed, and logic follows the new name.
renames :: [Item] -> M.Map Text Text
renames items = snd (foldl step ([], M.empty) ids)
  where
    ids = nub (concatMap nameOf items)
    -- The items of a multiple short text are sub-questions, named by it.
    nameOf = \case
      ItemQuestion q -> [qId q]
      ItemNote i _ _ -> [i]
      ItemGroup g
        | isMultiText g -> [groupId g]
        | otherwise -> [groupId g | Just _ <- [groupTitle g], groupKind g /= Page] ++ concatMap nameOf (groupItems g)
      ItemRaw _ _ -> []
    step (used, m) i =
      let c0 = T.filter isAsciiAlnum i
          c1 = if maybe False (isAlpha . fst) (T.uncons c0) then c0 else "q" <> c0
          c2 = T.take 20 c1
          fresh = head' [c | n <- [0 :: Int ..], let c = if n == 0 then c2 else T.take (20 - T.length (tshow n)) c2 <> tshow n, T.toLower c `notElem` used]
       in (T.toLower fresh : used, if fresh == i then m else M.insert i fresh m)
    head' = \case
      (x : _) -> x
      [] -> ""

isAsciiAlnum :: Char -> Bool
isAsciiAlnum c = isAsciiLower c || isAsciiUpper c || isDigit c

renameExpr :: M.Map Text Text -> Expr -> Expr
renameExpr m
  | M.null m = id
  | otherwise = go
  where
    go e = case e of
      ERef r -> ERef (ren r)
      ESelected q v -> ESelected (ren q) v
      EAnswered q -> EAnswered (ren q)
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      _ -> e
    ren r = case M.lookup r m of
      Just n -> n
      Nothing -> case [n <> "_" <> rest | (o, n) <- M.toList m, Just rest <- [T.stripPrefix (o <> "_") r]] of
        (x : _) -> x
        [] -> r

rename' :: M.Map Text Text -> Text -> Text
rename' m i = M.findWithDefault i i m

-- | A section of plain short-text or numeric questions that reads as
-- LimeSurvey's multiple short text / multiple numeric.
isMultiText :: Group -> Bool
isMultiText g =
  groupKind g == Section
    && textAttr "limesurvey:type" (groupAttrs g) `elem` [Just "Q", Just "K"]
    && not (null (groupItems g))
    && all simple (groupItems g)
  where
    simple = \case
      ItemQuestion q -> T.isPrefixOf (groupId g <> "_") (qId q) && isTextOrNum (qType q)
      _ -> False
    isTextOrNum = \case
      QText ShortText -> True
      QNumber _ -> True
      _ -> False

itemQuestions :: M.Map Text Text -> Item -> Conv [LsQuestion]
itemQuestions rename = \case
  ItemRaw (Format f) _ -> [] <$ warn "limesurvey" ("raw " <> f <> " content dropped")
  ItemNote i t rel -> do
    r <- printLogic limesurvey i (renameExpr rename <$> rel)
    pure [(lsQuestion "X" (rename' rename i)) {lqText = t, lqRelevance = fromMaybe "1" r}]
  ItemGroup g
    | isMultiText g -> pure <$> multiText rename g
    | otherwise -> do
        warn (groupId g) $ case groupKind g of
          Repeat -> "repeat group flattened; LimeSurvey has no loops"
          Page -> "nested page flattened into its group"
          Section -> "section flattened into its group"
        let header = [ItemNote (groupId g) t Nothing | Just t <- [groupTitle g], groupKind g /= Page]
        concat <$> mapM (itemQuestions rename . withRelevant (groupRelevant g)) (header ++ groupItems g)
  ItemQuestion q -> pure <$> writeQuestion rename q

multiText :: M.Map Text Text -> Group -> Conv LsQuestion
multiText rename g = do
  let title = rename' rename (groupId g)
      qs = [q | ItemQuestion q <- groupItems g]
      ty = fromMaybe "Q" (textAttr "limesurvey:type" (groupAttrs g))
  rel <- printLogic limesurvey title (renameExpr rename <$> groupRelevant g)
  subs <- forM qs $ \q -> do
    r <- printLogic limesurvey (qId q) (renameExpr rename <$> qRelevant q)
    let code = fromMaybe (qId q) (T.stripPrefix (groupId g <> "_") (qId q))
    pure (LsSub "" code 0 (qLabel q) (fromMaybe "" r) Nothing)
  when (length (nub (map qRequired qs)) > 1) $ warn title "per-item required flags merged into one"
  pure
    (lsQuestion ty title)
      { lqText = fromMaybe noText (groupTitle g)
      , lqHelp = fromMaybe noText (decode =<< M.lookup "limesurvey:help" (groupAttrs g))
      , lqRelevance = fromMaybe "1" rel
      , lqMandatory = if any qRequired qs then "Y" else "N"
      , lqFields = [(K.toText k, v) | Just (Object o) <- [M.lookup "limesurvey:fields" (groupAttrs g)], (k, String v) <- KM.toList o]
      , lqAttrs = storedAttrs (groupAttrs g)
      , lqSubs = subs
      }

-- | Group logic has nowhere to live once a group is flattened, so it is
-- pushed onto each child.
withRelevant :: Maybe Expr -> Item -> Item
withRelevant Nothing i = i
withRelevant (Just e) i = case i of
  ItemQuestion q -> ItemQuestion q {qRelevant = Just (maybe e (EBin And e) (qRelevant q))}
  ItemNote n t r -> ItemNote n t (Just (maybe e (EBin And e) r))
  ItemGroup g -> ItemGroup g {groupRelevant = Just (maybe e (EBin And e) (groupRelevant g))}
  _ -> i

storedAttrs :: Attrs -> [(Text, I18n)]
storedAttrs as = case M.lookup "limesurvey:attributes" as of
  Just (Object o) -> [(K.toText k, v) | (k, j) <- KM.toList o, Just v <- [decode j]]
  _ -> []

writeQuestion :: M.Map Text Text -> Question -> Conv LsQuestion
writeQuestion rename q = do
  rel <- printLogic limesurvey title (renameExpr rename <$> qRelevant q)
  (ty, fill) <- typed
  vattrs <- validationAttrs ty
  let fields = case M.lookup "limesurvey:fields" (qAttrs q) of
        Just (Object o) -> [(K.toText k, v) | (k, String v) <- KM.toList o]
        _ -> []
      q0 =
        (lsQuestion ty title)
          { lqText = qLabel q
          , lqHelp = fromMaybe noText (qHint q)
          , lqRelevance = fromMaybe "1" rel
          , lqPreg = fromMaybe "" (textAttr "limesurvey:preg" (qAttrs q))
          , lqMandatory = if qRequired q then (if stored == Just "S" then "S" else "Y") else "N"
          , lqTheme = fromMaybe "" (textAttr "limesurvey:theme" (qAttrs q))
          , lqDefault = plain <$> qDefault q
          , lqFields = fields
          , lqAttrs = storedAttrs (qAttrs q)
          }
      q1 = fill q0
      q2 = q1 {lqAttrs = sortOn fst (M.toList (M.union (M.fromList vattrs) (M.fromList (lqAttrs q1))))}
  checkCodes q2
  pure q2
  where
    title = rename' rename (qId q)
    storedType = textAttr "limesurvey:type" (qAttrs q)
    stored = textAttr "limesurvey:mandatory" (qAttrs q)
    setAttr k v x = x {lqAttrs = (k, v) : filter ((/= k) . fst) (lqAttrs x)}
    assessed = case M.lookup "limesurvey:assessment_values" (qAttrs q) of
      Just (Object o) -> [(K.toText k, v) | (k, String v) <- KM.toList o]
      _ -> []
    answers n cs = [LsAnswer (choiceValue c) n (choiceLabel c) (fromMaybe "" (lookup (choiceValue c) assessed)) | c <- cs]
    subs n cs = [LsSub "" (choiceValue c) n (choiceLabel c) "" Nothing | c <- cs]
    withOther cs x = case [c | c <- cs, choiceOther c] of
      [] -> pure x
      (o : more) -> do
        unless (null more) $ warn title "only one \"other\" option kept"
        warnFixedCodes title (Just "other") Nothing [o]
        pure . (if choiceLabel o == plain "Other" then id else setAttr "other_replace_text" (choiceLabel o)) $ x {lqOther = "Y"}
    regular = filter (not . choiceOther)
    typed :: Conv (Text, LsQuestion -> LsQuestion)
    typed = case qType q of
      QText ShortText -> pure ("S", id)
      QText LongText -> pure (if storedType == Just "U" then "U" else "T", id)
      QText k -> ("S", id) <$ warn title (T.toLower (tshow k) <> " validation dropped, written as short text")
      QNumber k -> pure ("N", if k == Integer then setAttr "num_value_int_only" (plain "1") else id)
      QChoice Single style cs
        | storedType == Just "G", map choiceValue cs == ["F", "M"] -> pure ("G", id)
        | otherwise -> do
            let ty = case (storedType, style) of
                  (Just "O", _) -> "O"
                  (_, Dropdown) -> "!"
                  _ -> "L"
                theme x = if style == Buttons && T.null (lqTheme x) then x {lqTheme = "bootstrap_buttons"} else x
            fill <- withOther cs (lsQuestion ty title)
            pure (ty, \x -> theme x {lqOther = lqOther fill, lqAttrs = lqAttrs fill ++ lqAttrs x, lqAnswers = answers 0 (regular cs)})
      QChoice Multiple style cs -> do
        when (style == Dropdown) $ warn title "multi-select dropdown written as checkboxes"
        let ty = if storedType == Just "P" then "P" else "M"
            excl = [choiceValue c | c <- regular cs, choiceExclusive c]
        fill <- withOther cs (lsQuestion ty title)
        pure
          ( ty
          , \x ->
              (if null excl then id else setAttr "exclude_all_others" (plain (T.intercalate ";" excl)))
                x {lqOther = lqOther fill, lqAttrs = lqAttrs fill ++ lqAttrs x, lqSubs = subs 0 (regular cs)}
          )
      QMatrix Single rows cols -> case storedType of
        Just t | t `elem` ["A", "B", "C", "E"], cols == fixedColumns t -> pure (t, \x -> x {lqSubs = subs 0 rows})
        Just t | t `elem` [":", ";"] -> pure (t, \x -> x {lqSubs = subs 0 rows ++ subs 1 cols})
        Just "1" -> do
          let scale1 = fromMaybe [] (decode =<< M.lookup "limesurvey:scale1" (qAttrs q))
          pure ("1", \x -> x {lqSubs = subs 0 rows, lqAnswers = answers 0 cols ++ answers 1 scale1})
        _ -> pure (if storedType == Just "H" then "H" else "F", \x -> x {lqSubs = subs 0 rows, lqAnswers = answers 0 cols})
      QMatrix Multiple rows cols ->
        pure (":", \x -> setAttr "multiflexible_checkbox" (plain "1") x {lqSubs = subs 0 rows ++ subs 1 cols})
      QScale 1 5 Nothing Nothing -> pure ("5", id)
      QScale lo hi l h
        | toInteger hi - toInteger lo >= toInteger maxScaleOptions -> do
            warn title ("rating scale from " <> tshow lo <> " to " <> tshow hi <> " has more than " <> tshow maxScaleOptions <> " points; written as a whole-number question with that range")
            when (isJust l || isJust h) $ warn title "scale end labels dropped"
            pure ("N", setAttr "num_value_int_only" (plain "1") . setAttr "max_num_value_n" (plain (tshow hi)) . setAttr "min_num_value_n" (plain (tshow lo)))
      QScale lo hi l h -> do
        warn title ("rating scale written as a list of " <> tshow (hi - lo + 1) <> " options")
        let label n
              | n == lo, Just t <- l = withNum n t
              | n == hi, Just t <- h = withNum n t
              | otherwise = plain (tshow n)
            withNum n (I18n d tr) = I18n (tshow n <> " - " <> d) (M.map ((tshow n <> " - ") <>) tr)
        pure ("L", \x -> x {lqAnswers = [LsAnswer (tshow n) 0 (label n) "" | n <- [lo .. hi]]})
      QRanking cs -> pure ("R", \x -> x {lqAnswers = answers 0 cs})
      QDate k ->
        let fmt = case k of
              Date -> Nothing
              Time -> Just "HH:MM"
              DateTime -> Just "yyyy-mm-dd HH:MM"
            has = isJust (lookup "date_format" (storedAttrs (qAttrs q)))
         in pure ("D", \x -> maybe x (\f -> if has then x else setAttr "date_format" (plain f) x) fmt)
      QBoolean -> pure ("Y", id)
      QFile -> pure ("|", id)
      QGeo -> ("S", id) <$ warn title "geopoint written as short text"
      QHidden -> pure ("S", setAttr "hidden" (plain "1"))
      QCalculate e -> do
        t <- printLogic limesurvey title (Just (renameExpr rename e))
        pure ("*", setAttr "equation" (plain ("{" <> fromMaybe "" t <> "}")))
      QOther "languageswitch" -> pure ("I", id)
      QOther t -> ("S", id) <$ warn title ("type " <> t <> " written as short text")
    validationAttrs ty = case qValidation q of
      Nothing -> pure []
      Just v -> do
        let range = [("min_num_value_n", plain (showNum n)) | Just n <- [valMin v]] ++ [("max_num_value_n", plain (showNum n)) | Just n <- [valMax v]]
        unless (ty == "N" || null range) $ warn title "min/max validation dropped (LimeSurvey sets it on numeric questions)"
        ex <- printLogic limesurvey title (substSelf title . renameExpr rename <$> valExpr v)
        pure $
          [r | ty == "N", r <- range]
            ++ [("em_validation_q", plain t) | Just t <- [ex]]
            ++ [("em_validation_q_tip", m) | Just m <- [valMessage v]]

-- | A rating scale becomes a list of radio options, one per point. Beyond
-- this many points it is written as a number question with the same range.
maxScaleOptions :: Int
maxScaleOptions = 1000

-- | Answer codes are at most five letters or digits, sub-question codes at
-- most twenty; LimeSurvey refuses to import anything else.
checkCodes :: LsQuestion -> Conv ()
checkCodes q = do
  forM_ (lqAnswers q) $ \a ->
    unless (T.all isAsciiAlnum (aCode a) && T.length (aCode a) `elem` [1 .. 5]) $
      warn (lqTitle q) ("answer code " <> aCode a <> " is not 1-5 letters or digits; LimeSurvey will reject it")
  forM_ (lqSubs q) $ \s ->
    unless (T.all isAsciiAlnum (sqCode s) && T.length (sqCode s) `elem` [1 .. 20]) $
      warn (lqTitle q) ("sub-question code " <> sqCode s <> " is not 1-20 letters or digits; LimeSurvey will reject it")
  forM_ (dupes [(aScale a, aCode a) | a <- lqAnswers q] ++ dupes [(sqScale s, sqCode s) | s <- lqSubs q]) $ \(_, c) ->
    warn (lqTitle q) ("code " <> c <> " is used twice; LimeSurvey will reject it")
  where
    -- Each repeated code once, in the order the repeats appear.
    dupes = go S.empty S.empty
      where
        go _ _ [] = []
        go seen told (x : xs)
          | S.member x seen, not (S.member x told) = x : go seen (S.insert x told) xs
          | otherwise = go (S.insert x seen) told xs

-- | Number every question and sub-question, in order.
numberIds :: [LsGroup] -> [LsGroup]
numberIds gs = reverse (snd (foldl group (1 :: Int, []) gs))
  where
    group (n, acc) g =
      let (n', qs) = foldl quest (n, []) (lgQuestions g)
       in (n', g {lgQuestions = reverse qs} : acc)
    quest (n, acc) q =
      let subs = [s {sqId = tshow i} | (i, s) <- zip [n + 1 ..] (lqSubs q)]
       in (n + 1 + length subs, q {lqId = tshow n, lqSubs = subs} : acc)

readNum :: Text -> Maybe Double
readNum t = case reads (T.unpack (T.strip t)) of
  [(d, "")] -> Just d
  _ -> Nothing

nonBlank :: Text -> Maybe Text
nonBlank t = if T.null (T.strip t) then Nothing else Just t

showNum :: Double -> Text
showNum n
  | n == fromIntegral r = tshow r
  | otherwise = tshow n
  where
    r = round n :: Integer

tshow :: Show a => a -> Text
tshow = T.pack . show
