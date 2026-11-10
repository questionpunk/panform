-- | SurveyJS Form Library JSON (@survey-core@ schema).
-- Pages hold @elements@; panels nest; strings are either plain or a
-- @{"default": ..., "fr": ...}@ locale map; logic is SurveyJS expressions.
module Panform.Format.SurveyJS (readSurveyJS, writeSurveyJS) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Design (designFields, readDesign, writeDesign)
import Panform.Expr
import Panform.Json

------------------------------------------------------------------------------
-- Reading

readSurveyJS :: BL.ByteString -> Conv Form
readSurveyJS bs = do
  v <- eitherDecode bs `orFail` "SurveyJS JSON"
  let pages = keyArr "pages" v
      top = if null pages then [v] else pages
      langs = case keyText "locale" v of
        Just l -> [l]
        Nothing -> []
  items <- forM (zip [1 :: Int ..] top) $ \(n, p) -> do
    children <- readElements (keyArr "elements" p ++ keyArr "questions" p)
    pure . ItemGroup $
      Group
        { groupId = keyTextOr ("page" <> T.pack (show n)) "name" p
        , groupKind = Page
        , groupTitle = key "title" p >>= loc
        , groupRelevant = parseOrRaw surveyjs <$> keyText "visibleIf" p
        , groupItems = children
        , -- Without pages the survey object is the page; its other
          -- properties are the survey's.
          groupAttrs = if null pages then M.empty else keepKeys ["name", "title", "visibleIf", "elements", "questions"] p
        }
  -- "surveyjs:design" is where the visual design is kept.
  when (isJust (key "design" v)) $ warn "surveyjs" "survey property design dropped"
  let meta =
        emptyMeta
          { metaTitle = key "title" v >>= loc
          , metaDescription = key "description" v >>= loc
          , metaAttrs = readDesign "surveyjs" v <> keepKeys ("design" : surveyKeys ++ designFields "surveyjs") v
          }
      survey = Form meta (if null pages then flatten items else items)
      used = usedLanguages survey
      -- SurveyJS's default locale is English unless "locale" says otherwise.
      allLangs = if null langs && null used then [] else nub ((if null langs then ["en"] else langs) ++ used)
  pure survey {formMeta = meta {metaLanguages = allLangs}}
  where
    flatten [ItemGroup g] = groupItems g
    flatten is = is

-- | Survey properties the IR holds.
surveyKeys :: [Text]
surveyKeys = ["title", "description", "locale", "pages", "elements", "questions"]

-- | Properties the IR has no field for (@enableIf@, @requiredIf@,
-- @choicesByUrl@, @triggers@, ...), kept as @surveyjs:<property>@ so a
-- SurveyJS round trip writes them back.
keepKeys :: [Text] -> Value -> Attrs
keepKeys known = \case
  Object o -> M.fromList [("surveyjs:" <> K.toText k, x) | (k, x) <- KM.toList o, K.toText k `notElem` known, x /= Null]
  _ -> M.empty

-- | Kept properties as fields, leaving out the ones a writer sets itself
-- and the given ones, which the writer decides from the IR.
keptFields :: [K.Key] -> [K.Key] -> Attrs -> [(K.Key, Value)]
keptFields skip written a =
  [ (k, x)
  | (k0, x) <- M.toList a
  , Just k <- [K.fromText <$> T.stripPrefix "surveyjs:" k0]
  , k `notElem` written
  , k `notElem` ("design" : skip)
  ]

-- | An object from written fields and kept properties; written fields win.
withKept :: Attrs -> [(K.Key, Value)] -> Value
withKept = withKeptBut []

withKeptBut :: [K.Key] -> Attrs -> [(K.Key, Value)] -> Value
withKeptBut skip a fields = Object (KM.fromList (keptFields skip (map fst fields) a ++ fields))

-- | The kept properties a question's writer decides from its type: the
-- choice lists of a question with choices, the input type of a text box.
-- On any other question they are plain properties (a comment box's
-- @rows@ is its height) and are written back as they were read.
decided :: QType -> [K.Key]
decided t =
  ["type", "validators", "defaultValue"]
    ++ ["inputType" | textBox]
    ++ (if hasChoices then ["choices", "rows", "columns", "rateValues"] else [])
  where
    textBox = case t of
      QText k -> k /= LongText
      QNumber _ -> True
      QDate _ -> True
      QHidden -> True
      _ -> False
    hasChoices = case t of
      QChoice {} -> True
      QMatrix {} -> True
      QRanking _ -> True
      _ -> False

objKeys :: Value -> [Text]
objKeys = \case
  Object o -> map K.toText (KM.keys o)
  _ -> []

-- | A SurveyJS localizable string.
loc :: Value -> Maybe I18n
loc = \case
  String t -> Just (plain t)
  Object o -> do
    let m = M.fromList [(K.toText k, t) | (k, String t) <- KM.toList o]
    d <- M.lookup "default" m `orElse` M.lookup "en" m `orElse` firstOf m
    Just (I18n d (M.delete "default" m))
  _ -> Nothing
  where
    orElse a b = maybe b Just a
    firstOf = fmap snd . M.lookupMin

readElements :: [Value] -> Conv [Item]
readElements = fmap catMaybes . mapM readElement

readElement :: Value -> Conv (Maybe Item)
readElement e = case keyText "type" e of
  Nothing -> Nothing <$ warn "surveyjs" "element without a type skipped"
  Just "html" -> do
    let lost = filter (`notElem` ["type", "name", "html", "visibleIf"]) (objKeys e)
    unless (null lost) $ warn name ("html element properties dropped: " <> T.intercalate ", " lost)
    pure . Just $ ItemNote name (fromMaybe (plain "") (key "html" e >>= loc)) visible
  Just t | t `elem` ["panel", "paneldynamic"] -> do
    kids <- readElements (keyArr "elements" e ++ keyArr "templateElements" e)
    pure . Just . ItemGroup $
      Group
        { groupId = name
        , groupKind = if t == "panel" then Section else Repeat
        , groupTitle = key "title" e >>= loc
        , groupRelevant = visible
        , groupItems = kids
        , groupAttrs = keepKeys ["type", "name", "title", "visibleIf", "elements", "templateElements"] e
        }
  Just t -> do
    qt <- readType t e
    let (validation, otherValidators) = readValidation qt e
        -- What the writer would not write back the same from the IR alone.
        special =
          [("surveyjs:validators", toJSON otherValidators) | not (null otherValidators)]
            ++ [("surveyjs:defaultValue", d) | Just d <- [key "defaultValue" e], case d of String _ -> False; _ -> True]
            ++ [("surveyjs:" <> k, raw) | k <- choiceKeys t e, Just raw <- [key k e], toJSON (map (choiceJSON . readChoice) (keyArr k e)) /= raw]
            ++ [("surveyjs:type", String t) | t `elem` ["imagepicker", "signaturepad"] || t == "rating" && not (null (keyArr "rateValues" e))]
            ++ [("surveyjs:inputType", String i) | t == "text", Just i <- [keyText "inputType" e], i `notElem` writtenInputTypes || qt == QHidden]
    pure . Just . ItemQuestion $
      Question
        { qId = name
        , qLabel = fromMaybe (plain name) (key "title" e >>= loc)
        , qHint = key "description" e >>= loc
        , qType = qt
        , qRequired = keyBool "isRequired" e == Just True
        , qRelevant = visible
        , qValidation = validation
        , qDefault = keyText "defaultValue" e
        , qAttrs = keepKeys (["type", "name", "title", "description", "isRequired", "visibleIf", "validators", "defaultValue"] ++ typeKeys t e) e <> M.fromList special
        }
  where
    name = keyTextOr "" "name" e
    visible = parseOrRaw surveyjs <$> keyText "visibleIf" e

-- | The @inputType@s the writer derives from a question's type.
writtenInputTypes :: [Text]
writtenInputTypes = ["text", "email", "url", "tel", "number", "date", "time", "datetime-local"]

-- | The properties holding a question type's choice lists.
choiceKeys :: Text -> Value -> [Text]
choiceKeys t e
  | t `elem` ["radiogroup", "dropdown", "checkbox", "tagbox", "imagepicker", "ranking"] = ["choices"]
  | t == "rating" && not (null (keyArr "rateValues" e)) = ["rateValues"]
  | t == "matrix" = ["rows", "columns"]
  | otherwise = []

-- | The properties 'readType' turns into the question's type.
typeKeys :: Text -> Value -> [Text]
typeKeys t e = choiceKeys t e ++ case t of
  "text" ->
    ["inputType"]
      ++ ["visible" | keyBool "visible" e == Just False]
      ++ ["step" | keyBool "visible" e /= Just False, keyText "inputType" e == Just "number", keyNum "step" e == Just 1]
  _ | t `elem` ["radiogroup", "dropdown", "checkbox", "tagbox", "imagepicker"] ->
    (if hasOther then ["hasOther", "showOtherItem", "otherText"] else [])
      ++ (if hasNone then ["hasNone", "showNoneItem", "noneText"] else [])
      ++ ["multiSelect" | t == "imagepicker"]
  "rating" | null (keyArr "rateValues" e) -> ["rateMin", "rateMax", "minRateDescription", "maxRateDescription"]
  "expression" -> ["expression"]
  _ -> []
  where
    hasOther = keyBool "hasOther" e == Just True || keyBool "showOtherItem" e == Just True
    hasNone = keyBool "hasNone" e == Just True || keyBool "showNoneItem" e == Just True

readType :: Text -> Value -> Conv QType
readType t e = case t of
  "text"
    | keyBool "visible" e == Just False -> pure QHidden
    | otherwise -> pure $ case keyTextOr "text" "inputType" e of
        "email" -> QText Email
        "url" -> QText Url
        "tel" -> QText Phone
        "number" -> QNumber (if keyNum "step" e == Just 1 then Integer else Decimal)
        "range" -> QNumber Decimal
        "date" -> QDate Date
        "time" -> QDate Time
        "datetime-local" -> QDate DateTime
        _ -> QText ShortText
  "comment" -> pure (QText LongText)
  "radiogroup" -> pure (QChoice Single Radio choices)
  "dropdown" -> pure (QChoice Single Dropdown choices)
  "checkbox" -> pure (QChoice Multiple Radio choices)
  "tagbox" -> pure (QChoice Multiple Dropdown choices)
  "imagepicker" ->
    pure (QChoice (if keyBool "multiSelect" e == Just True then Multiple else Single) Buttons choices)
  "boolean" -> pure QBoolean
  "ranking" -> pure (QRanking choices)
  "rating" -> case keyArr "rateValues" e of
    [] ->
      pure $
        QScale
          (maybe 1 round (keyNum "rateMin" e))
          (maybe 5 round (keyNum "rateMax" e))
          (key "minRateDescription" e >>= loc)
          (key "maxRateDescription" e >>= loc)
    vs -> pure (QChoice Single Buttons (map readChoice vs))
  "matrix" ->
    pure $
      QMatrix
        Single
        (map readChoice (keyArr "rows" e))
        (map readChoice (keyArr "columns" e))
  "file" -> pure QFile
  "signaturepad" -> pure QFile
  "expression" -> pure (QCalculate (parseOrRaw surveyjs (keyTextOr "" "expression" e)))
  other -> QOther other <$ warn ("surveyjs:" <> keyTextOr "" "name" e) ("unsupported type " <> other <> " kept by name")
  where
    choices =
      map readChoice (keyArr "choices" e)
        ++ [Choice "other" (fromMaybe (plain "Other") (key "otherText" e >>= loc)) True False | keyBool "hasOther" e == Just True || keyBool "showOtherItem" e == Just True]
        ++ [Choice "none" (fromMaybe (plain "None") (key "noneText" e >>= loc)) False True | keyBool "hasNone" e == Just True || keyBool "showNoneItem" e == Just True]

readChoice :: Value -> Choice
readChoice v = case v of
  Object _ ->
    let val = fromMaybe "" (keyText "value" v)
     in Choice val (fromMaybe (plain val) (key "text" v >>= loc)) False False
  _ -> let val = fromMaybe "" (asText v) in choice val val

-- | The validator a bound is written as: a length on a text question, a
-- value on any other.
boundValidator :: QType -> (Text, Text, Text)
boundValidator = \case
  QText _ -> ("text", "minLength", "maxLength")
  _ -> ("numeric", "minValue", "maxValue")

-- | Bounds, an expression and their message from the validators, and the
-- validators the IR cannot hold (regex, email, a numeric check on a text
-- answer, a second message), which are kept as they are.
readValidation :: QType -> Value -> (Maybe Validation, [Value])
readValidation qt e = (validation, rest)
  where
    (kind, minK, maxK) = boundValidator qt
    only ks v = all (`elem` ks) (objKeys v)
    isBound v = keyText "type" v == Just kind && only ["type", minK, maxK, "text"] v && (isJust (keyNum minK v) || isJust (keyNum maxK v))
    isExpr v = keyText "type" v == Just "expression" && only ["type", "expression", "text"] v && isJust (keyText "expression" v)
    takeFirst p xs = case break p xs of
      (pre, x : post) -> (Just x, pre ++ post)
      _ -> (Nothing, xs)
    (bound, rest0) = takeFirst isBound (keyArr "validators" e)
    message v = v >>= key "text" >>= loc
    -- One message serves both rules; an expression with another message
    -- of its own stays as it is.
    sameMessage x = isNothing bound || message (Just x) == message bound
    (expr, rest) = takeFirst (\x -> isExpr x && sameMessage x) rest0
    validation
      | isNothing bound && isNothing expr = Nothing
      | otherwise =
          Just
            Validation
              { valMin = bound >>= keyNum minK
              , valMax = bound >>= keyNum maxK
              , valExpr = parseOrRaw surveyjs <$> (expr >>= keyText "expression")
              , valMessage = maybe (message expr) Just (message bound)
              }

------------------------------------------------------------------------------
-- Writing

writeSurveyJS :: Form -> Conv BL.ByteString
writeSurveyJS s@(Form meta items) = do
  -- SurveyJS has no language list; languages are inferred from the texts.
  case filter (`notElem` usedLanguages s) (drop 1 (metaLanguages meta)) of
    [] -> pure ()
    ls -> warn "surveyjs" ("declared languages with no translations dropped: " <> T.intercalate ", " ls)
  pages <- forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    els <- concat <$> mapM writeItem kids
    vis <- printLogic surveyjs ("page " <> groupIdOr n g) (g >>= groupRelevant)
    pure . withKept (maybe M.empty groupAttrs g) . catMaybes $
      [ "name" .=? String (groupIdOr n g)
      , ("title" .=?) . locJSON =<< (g >>= groupTitle)
      , ("visibleIf" .=?) . String =<< vis
      , "elements" .=? toJSON els
      ]
  pure . encodePretty . withKept (metaAttrs meta) $
    catMaybes
      [ ("title" .=?) . locJSON =<< metaTitle meta
      , ("description" .=?) . locJSON =<< metaDescription meta
      , case metaLanguages meta of
          (l : _) | l /= "en" -> "locale" .=? String l
          _ -> Nothing
      , "pages" .=? toJSON pages
      ]
      ++ writeDesign "surveyjs" meta
  where
    groupIdOr n = maybe ("page" <> T.pack (show n)) groupId

-- | Top-level pages as they are; loose items between pages become a page.
paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

locJSON :: I18n -> Value
locJSON (I18n d tr)
  | M.null tr = String d
  | otherwise = toJSON (M.insert "default" d tr)

writeItem :: Item -> Conv [Value]
writeItem = \case
  ItemNote i t rel -> do
    vis <- printLogic surveyjs i rel
    pure [obj ["type" .=? "html", "name" .=? String i, "html" .=? locJSON t, ("visibleIf" .=?) . String =<< vis]]
  ItemRaw (Format "surveyjs") v -> pure [v]
  ItemRaw (Format f) _ -> [] <$ warn "surveyjs" ("raw " <> f <> " content dropped")
  ItemGroup g -> do
    kids <- concat <$> mapM writeItem (groupItems g)
    vis <- printLogic surveyjs (groupId g) (groupRelevant g)
    unless (groupKind g /= Page) $ warn (groupId g) "nested page written as a panel"
    let (ty, field) = if groupKind g == Repeat then ("paneldynamic", "templateElements") else ("panel", "elements")
    pure
      [ withKept (groupAttrs g) . catMaybes $
          [ "type" .=? ty
          , "name" .=? String (groupId g)
          , ("title" .=?) . locJSON =<< groupTitle g
          , ("visibleIf" .=?) . String =<< vis
          , K.fromText field .=? toJSON kids
          ]
      ]
  ItemQuestion q -> pure <$> writeQuestion q

writeQuestion :: Question -> Conv Value
writeQuestion q = do
  vis <- printLogic surveyjs (qId q) (qRelevant q)
  typeFields <- questionFields q
  val <- writeValidation q
  -- A default that was not a string (a number, a boolean, a list) is
  -- written as it was read while it still says the same thing.
  let dflt = case M.lookup "surveyjs:defaultValue" (qAttrs q) of
        Just raw | asText raw == qDefault q -> Just raw
        _ -> String <$> qDefault q
  pure . withKeptBut (decided (qType q)) (qAttrs q) $
    catMaybes
      [ "name" .=? String (qId q)
      , "title" .=? locJSON (qLabel q)
      , ("description" .=?) . locJSON =<< qHint q
      , if qRequired q then "isRequired" .=? Bool True else Nothing
      , ("visibleIf" .=?) . String =<< vis
      , ("defaultValue" .=?) =<< dflt
      , ("validators" .=?) =<< val
      ]
      ++ typeFields

questionFields :: Question -> Conv [(K.Key, Value)]
questionFields q = case qType q of
  QText ShortText | Just i <- kept "inputType" -> pure [("type", "text"), ("inputType", i)]
  QText k -> pure $ case k of
    LongText -> [("type", "comment")]
    Email -> [("type", "text"), ("inputType", "email")]
    Url -> [("type", "text"), ("inputType", "url")]
    Phone -> [("type", "text"), ("inputType", "tel")]
    ShortText -> [("type", "text")]
  QNumber Integer -> pure [("type", "text"), ("inputType", "number"), ("step", Number 1)]
  QNumber Decimal | Just i <- kept "inputType" -> pure [("type", "text"), ("inputType", i)]
  QNumber Decimal -> pure [("type", "text"), ("inputType", "number")]
  QChoice Single Buttons cs | kept "type" == Just "rating", not (any (\c -> choiceOther c || choiceExclusive c) cs) -> pure [("type", "rating"), ("rateValues", choicesJSON q "rateValues" cs)]
  QChoice sel Buttons cs | kept "type" == Just "imagepicker" -> do
    warnFixedCodes (qId q) (Just "other") (Just "none") cs
    pure ([("type", "imagepicker")] ++ [("multiSelect", Bool True) | sel == Multiple] ++ choiceFields q cs)
  QDate k -> pure [("type", "text"), ("inputType", case k of Date -> "date"; Time -> "time"; DateTime -> "datetime-local")]
  QChoice sel style cs ->
    let ty = case (sel, style) of
          (Single, Dropdown) -> "dropdown"
          (Multiple, Dropdown) -> "tagbox"
          (Single, _) -> "radiogroup"
          (Multiple, _) -> "checkbox"
     in do
          warnFixedCodes (qId q) (Just "other") (Just "none") cs
          when (length (filter choiceOther cs) > 1) $ warn (qId q) "only the first \"other\" option kept as SurveyJS's other item"
          when (length (filter (\c -> choiceExclusive c && not (choiceOther c)) cs) > 1) $ warn (qId q) "only the first exclusive option kept as SurveyJS's none item"
          pure (("type", ty) : choiceFields q cs)
  QMatrix sel rows cols -> do
    unless (sel == Single) $ warn (qId q) "multi-select matrix written as single-select"
    pure [("type", "matrix"), ("rows", choicesJSON q "rows" rows), ("columns", choicesJSON q "columns" cols)]
  QScale lo hi l h ->
    pure $
      [("type", "rating"), ("rateMin", toJSON lo), ("rateMax", toJSON hi)]
        ++ [("minRateDescription", locJSON t) | Just t <- [l]]
        ++ [("maxRateDescription", locJSON t) | Just t <- [h]]
  QRanking cs -> pure [("type", "ranking"), ("choices", choicesJSON q "choices" cs)]
  QBoolean -> pure [("type", "boolean")]
  QFile | kept "type" == Just "signaturepad" -> pure [("type", "signaturepad")]
  QFile -> pure [("type", "file")]
  QGeo -> [("type", "text")] <$ warn (qId q) "geopoint written as a text box"
  QHidden -> pure ([("type", "text"), ("visible", Bool False)] ++ [("inputType", i) | Just i <- [kept "inputType"]])
  QCalculate e -> do
    t <- printLogic surveyjs (qId q) (Just e)
    pure [("type", "expression"), ("expression", String (fromMaybe "" t))]
  QOther t -> pure [("type", String t)]
  where
    kept k = M.lookup ("surveyjs:" <> k) (qAttrs q)

-- | A choice list. Entries that carried more than a value and a text (a
-- per-choice @visibleIf@, an image link, a number as the value) are
-- written as they were read while they still name the same choices.
choicesJSON :: Question -> Text -> [Choice] -> Value
choicesJSON q k cs = case M.lookup ("surveyjs:" <> k) (qAttrs q) of
  Just raw@(Array a) | map readChoice (V.toList a) == cs -> raw
  _ -> toJSON (map choiceJSON cs)

-- | Plain choices; an "other" or exclusive choice becomes SurveyJS's
-- built-in other/none item when it uses the conventional value.
choiceFields :: Question -> [Choice] -> [(K.Key, Value)]
choiceFields q cs =
  [("choices", choicesJSON q "choices" regular)]
    ++ concat [[("showOtherItem", Bool True), ("otherText", locJSON (choiceLabel c))] | c <- others]
    ++ concat [[("showNoneItem", Bool True), ("noneText", locJSON (choiceLabel c))] | c <- nones]
  where
    others = take 1 [c | c <- cs, choiceOther c]
    nones = take 1 [c | c <- cs, choiceExclusive c, not (choiceOther c)]
    regular = [c | c <- cs, not (c `elem` others || c `elem` nones)]

choiceJSON :: Choice -> Value
choiceJSON c
  | choiceLabel c == plain (choiceValue c) = String (choiceValue c)
  | otherwise = object ["value" .= choiceValue c, "text" .= locJSON (choiceLabel c)]

writeValidation :: Question -> Conv (Maybe Value)
writeValidation q = do
  generated <- case qValidation q of
    Nothing -> pure []
    Just v -> do
      ex <- printLogic surveyjs (qId q) (valExpr v)
      let msg = ("text" .=?) . locJSON =<< valMessage v
          (kind, minK, maxK) = boundValidator (qType q)
          -- A bound on a text question limits its length, not its value.
          bound =
            [ obj ["type" .=? String kind, (K.fromText minK .=?) . toJSON =<< valMin v, (K.fromText maxK .=?) . toJSON =<< valMax v, msg]
            | isJust (valMin v) || isJust (valMax v)
            ]
          expr = [obj ["type" .=? "expression", "expression" .=? String t, msg] | Just t <- [ex]]
      when (null (bound ++ expr) && isJust (valMessage v)) $ warn (qId q) "validation message without a rule dropped"
      pure (bound ++ expr)
  let kept = case M.lookup "surveyjs:validators" (qAttrs q) of
        Just (Array a) -> V.toList a
        _ -> []
  pure $ case generated ++ kept of
    [] -> Nothing
    vs -> Just (toJSON vs)
