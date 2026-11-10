-- | HL7 FHIR Questionnaire (R4 and R5 JSON) with the Structured Data
-- Capture extensions renderers rely on: itemControl, hidden, min/maxValue,
-- optionExclusive, calculatedExpression and the translation extension on
-- @_text@. Items nest; @enableWhen@ conditions map to 'Expr', and FHIRPath
-- expressions are kept raw in the @fhirpath@ dialect.
module Panform.Format.FHIR (readFHIR, writeFHIR) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (nub, partition)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Json

------------------------------------------------------------------------------
-- Extension URLs

extItemControl, extHidden, extMin, extMax, extStep, extExclusive, extOpenLabel, extCalc, extEnableExpr, extConstraint, extTranslation, controlSystem :: Text
extItemControl = "http://hl7.org/fhir/StructureDefinition/questionnaire-itemControl"
extHidden = "http://hl7.org/fhir/StructureDefinition/questionnaire-hidden"
extMin = "http://hl7.org/fhir/StructureDefinition/minValue"
extMax = "http://hl7.org/fhir/StructureDefinition/maxValue"
extStep = "http://hl7.org/fhir/StructureDefinition/questionnaire-sliderStepValue"
extExclusive = "http://hl7.org/fhir/StructureDefinition/questionnaire-optionExclusive"
extOpenLabel = "http://hl7.org/fhir/uv/sdc/StructureDefinition/sdc-questionnaire-openLabel"
extCalc = "http://hl7.org/fhir/uv/sdc/StructureDefinition/sdc-questionnaire-calculatedExpression"
extEnableExpr = "http://hl7.org/fhir/uv/sdc/StructureDefinition/sdc-questionnaire-enableWhenExpression"
extConstraint = "http://hl7.org/fhir/StructureDefinition/targetConstraint"
extTranslation = "http://hl7.org/fhir/StructureDefinition/translation"
controlSystem = "http://hl7.org/fhir/questionnaire-item-control"

------------------------------------------------------------------------------
-- JSON helpers

extensions :: Value -> [Value]
extensions = keyArr "extension"

extUrl :: Value -> Text
extUrl = keyTextOr "" "url"

extNamed :: Text -> Value -> Maybe Value
extNamed u v = listToMaybe [e | e <- extensions v, extUrl e == u]

-- | The @value[x]@ of an extension or answer, whatever its type suffix.
valueX :: Text -> Value -> Maybe (Text, Value)
valueX prefix (Object o) = listToMaybe [(t, v) | (k, v) <- KM.toList o, Just t <- [T.stripPrefix prefix (K.toText k)], not (T.null t), v /= Null]
valueX _ _ = Nothing

subExt :: Text -> Value -> Maybe Value
subExt u e = listToMaybe [s | s <- extensions e, extUrl s == u]

itemControl :: Value -> Maybe Text
itemControl v = do
  e <- extNamed extItemControl v
  cc <- key "valueCodeableConcept" e
  listToMaybe (keyArr "coding" cc) >>= keyText "code"

isTrue :: Maybe Value -> Bool
isTrue = maybe False (\e -> key "valueBoolean" e == Just (Bool True))

-- | A string with its translations from the sibling @_key@ element.
i18nAt :: Text -> Value -> Maybe I18n
i18nAt k v = do
  d <- keyText k v
  let tr =
        M.fromList
          [ (l, c)
          | Just ev <- [key ("_" <> k) v]
          , e <- extensions ev
          , extUrl e == extTranslation
          , let sub = extensions e
          , Just l <- [listToMaybe [x | s <- sub, extUrl s == "lang", Just (_, String x) <- [valueX "value" s]]]
          , Just c <- [listToMaybe [x | s <- sub, extUrl s == "content", Just (_, String x) <- [valueX "value" s]]]
          ]
  pure (I18n d tr)

-- | @key@ and, when translated, @_key@.
i18nFields :: Text -> I18n -> [(K.Key, Value)]
i18nFields k (I18n d tr) =
  (K.fromText k, String d)
    : [ ( K.fromText ("_" <> k)
        , object
            [ "extension"
                .= [ object
                       [ "url" .= extTranslation
                       , "extension" .= [object ["url" .= ("lang" :: Text), "valueCode" .= l], object ["url" .= ("content" :: Text), "valueString" .= t]]
                       ]
                   | (l, t) <- M.toList tr
                   ]
            ]
        )
      | not (M.null tr)
      ]

controlExt :: Text -> Value
controlExt c =
  object
    [ "url" .= extItemControl
    , "valueCodeableConcept" .= object ["coding" .= [object ["system" .= controlSystem, "code" .= c]]]
    ]

boolExt :: Text -> Value
boolExt u = object ["url" .= u, "valueBoolean" .= True]

numValue :: Double -> Value
numValue = Number . fromFloatDigits

isIntegral :: Double -> Bool
isIntegral x = x == fromIntegral (round x :: Integer)

-- | Remaining object fields, kept as @fhir:key@ attributes.
keepFields :: [Text] -> Value -> Attrs
keepFields consumed (Object o) = M.fromList [("fhir:" <> K.toText k, v) | (k, v) <- KM.toList o, K.toText k `notElem` consumed, v /= Null]
keepFields _ _ = M.empty

-- | Preserved attributes written back as fields; special ones are handled
-- by the writer itself.
attrFields :: Attrs -> [(K.Key, Value)]
attrFields a = [(K.fromText k, v) | (k0, v) <- M.toList a, Just k <- [T.stripPrefix "fhir:" k0], k `notElem` special]
  where
    special = ["extension", "system", "answerType", "type", "visible", "itemControl", "hintId", "hintControl", "rowIds", "_text", "calculatedExpression", "enableWhenExpression", "targetConstraint"]

-- | An SDC expression as the writer spells it.
fhirpathObj :: Text -> Value
fhirpathObj src = object ["language" .= ("text/fhirpath" :: Text), "expression" .= src]

-- | An expression read with more than its FHIRPath text (a name, a
-- description, another language), kept so it can be written back.
keepExpr :: Text -> Maybe Value -> Attrs
keepExpr k = \case
  Just o | o /= fhirpathObj (keyTextOr "" "expression" o) -> M.singleton k o
  _ -> M.empty

-- | The expression object for a FHIRPath text: the one that was read
-- while its text is unchanged, else a plain one.
exprObj :: Attrs -> Text -> Text -> Value
exprObj attrs k src = case M.lookup k attrs of
  Just o | keyText "expression" o == Just src -> o
  _ -> fhirpathObj src

-- | A target constraint as the writer spells it.
constraintExt :: Text -> Text -> Maybe Text -> Value
constraintExt qid src human =
  object
    [ "url" .= extConstraint
    , "extension"
        .= ( [ object ["url" .= ("key" :: Text), "valueId" .= (qid <> "-1")]
             , object ["url" .= ("severity" :: Text), "valueCode" .= ("error" :: Text)]
             , object ["url" .= ("expression" :: Text), "valueExpression" .= fhirpathObj src]
             ]
               ++ [object ["url" .= ("human" :: Text), "valueString" .= m] | Just m <- [human]]
           )
    ]

-- | A target constraint's FHIRPath text and its message.
constraintParts :: Value -> (Maybe Text, Maybe I18n)
constraintParts c = (subExt "expression" c >>= key "valueExpression" >>= keyText "expression", subExt "human" c >>= i18nAt "valueString")

-- | What a sibling @_key@ element holds besides translations (rendering
-- extensions and the like).
underscoreRest :: Text -> Value -> Attrs
underscoreRest k v = case key ("_" <> k) v of
  Just (Object o) ->
    let exts = [e | e <- extensions (Object o), extUrl e /= extTranslation]
        o' = (if null exts then KM.delete "extension" else KM.insert "extension" (toJSON exts)) o
     in M.fromList [("fhir:_" <> k, Object o') | not (KM.null o')]
  _ -> M.empty

------------------------------------------------------------------------------
-- Reading

data Ctx = Ctx
  { cValueSets :: M.Map Text [Choice]
  , cVsSystems :: M.Map Text Text
  -- ^ The one code system a contained ValueSet draws on.
  , cMulti :: S.Set Text
  -- ^ linkIds of multi-select choice questions.
  }

readFHIR :: BL.ByteString -> Conv Form
readFHIR bs = do
  v <- eitherDecode bs `orFail` "FHIR JSON"
  unless (keyText "resourceType" v == Just "Questionnaire") $ failWith "not a FHIR Questionnaire resource"
  let contained = keyArr "contained" v
      valueSets = valueSetChoices contained
      everything = walk (keyArr "item" v)
      walk is = is ++ concatMap (walk . keyArr "item") is
      multi = S.fromList [l | i <- everything, keyTextOr "" "type" i `elem` choiceTypes, keyBool "repeats" i == Just True, Just l <- [keyText "linkId" i]]
      vsSystems = M.fromList [("#" <> i, sys) | vs <- contained, keyText "resourceType" vs == Just "ValueSet", Just i <- [keyText "id" vs], [sys] <- [vsSystem vs]]
      ctx = Ctx valueSets vsSystems multi
      usedSets = S.fromList (mapMaybe (keyText "answerValueSet") everything)
  -- A value set an item draws on stays in @contained@, and the item keeps
  -- its reference, so nothing of the set is lost; the options listed from
  -- it are for the other formats.
  mapM_ (\(i, why) -> warn i ("options from this value set may be incomplete or ambiguous: " <> why)) $
    [ (i, why)
    | vs <- contained
    , keyText "resourceType" vs == Just "ValueSet"
    , Just i <- [("#" <>) <$> keyText "id" vs]
    , S.member i usedSets
    , Just why <- [vsPartial vs]
    ]
  items <- concat <$> mapM (readItem ctx) (keyArr "item" v)
  let title = i18nAt "title" v
      desc = i18nAt "description" v
      langs = nub (maybe [] pure (keyText "language" v) ++ usedLanguages (Form emptyMeta {metaTitle = title, metaDescription = desc} items))
      meta =
        emptyMeta
          { metaId = keyText "name" v `orElse` keyText "id" v
          , metaTitle = title
          , metaDescription = desc
          , metaLanguages = if isJust (keyText "language" v) then langs else []
          , metaAttrs =
              keepFields ["resourceType", "item", "title", "_title", "description", "_description", "language", "contained"] v
                <> M.fromList [("fhir:contained", toJSON contained) | not (null contained)]
          }
  pure (Form meta items)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse a b = maybe b Just a

choiceTypes :: [Text]
choiceTypes = ["choice", "open-choice", "coding"]

-- | Codes of a contained ValueSet, from its expansion or its compose.
vsSystem :: Value -> [Text]
vsSystem vs =
  nub $
    mapMaybe (keyText "system") (maybe [] (keyArr "include") (key "compose" vs))
      ++ mapMaybe (keyText "system") (maybe [] (keyArr "contains") (key "expansion" vs))

-- | The code system of options that come from a contained ValueSet.
vsAttrs :: Ctx -> Value -> Attrs
vsAttrs ctx v = M.fromList [("fhir:system", String sys) | null (keyArr "answerOption" v), Just vs <- [keyText "answerValueSet" v], Just sys <- [M.lookup vs (cVsSystems ctx)]]

-- | Why the options listed from a value set are not the whole story.
vsPartial :: Value -> Maybe Text
vsPartial vs
  | not (null expansion) = if any (not . null . keyArr "contains") expansion then Just "nested codes" else dup
  | not (null (maybe [] (keyArr "exclude") (key "compose" vs))) = Just "exclusions"
  | any (\inc -> not (null (keyArr "filter" inc)) || not (null (keyArr "valueSet" inc)) || null (keyArr "concept" inc)) includes = Just "filters or whole code systems"
  | otherwise = dup
  where
    expansion = keyArr "contains" =<< maybe [] pure (key "expansion" vs)
    includes = maybe [] (keyArr "include") (key "compose" vs)
    codes = map choiceValue (vsChoices vs)
    dup = if length (nub codes) /= length codes then Just "one code in several systems" else Nothing

-- | Contained value sets as option lists, by @#id@.
valueSetChoices :: [Value] -> M.Map Text [Choice]
valueSetChoices contained = M.fromList [("#" <> i, vsChoices vs) | vs <- contained, keyText "resourceType" vs == Just "ValueSet", Just i <- [keyText "id" vs]]

vsChoices :: Value -> [Choice]
vsChoices vs = case keyArr "contains" =<< maybe [] pure (key "expansion" vs) of
  [] -> [concept c | inc <- maybe [] (keyArr "include") (key "compose" vs), c <- keyArr "concept" inc]
  cs -> map concept cs
  where
    concept c = let code = keyTextOr "" "code" c in Choice code (fromMaybe (plain code) (i18nAt "display" c)) False False

-- | One Questionnaire item. Items nested under a question come back as
-- siblings after it.
readItem :: Ctx -> Value -> Conv [Item]
readItem ctx v = do
  let linkId = keyTextOr "" "linkId" v
      ty = keyTextOr "string" "type" v
      text = fromMaybe (plain "") (i18nAt "text" v)
  rel <- readEnableWhen ctx v
  case ty of
    "display" -> do
      nested <- concat <$> mapM (readItem ctx) (keyArr "item" v)
      unless (null nested) $ warn linkId "items nested under a display item moved after it"
      -- A note is its text and its condition; anything else has no place.
      let lost =
            filter (`notElem` ["linkId", "text", "_text", "type", "enableWhen", "enableBehavior", "item", "extension"]) (objKeys v)
              ++ [extUrl e | e <- extensions v, extUrl e /= extEnableExpr]
              ++ ["_text" | not (M.null (underscoreRest "text" v))]
              ++ ["enableWhenExpression details" | not (M.null (keepExpr "x" (extNamed extEnableExpr v >>= key "valueExpression")))]
      unless (null lost) $ warn linkId ("display item fields dropped: " <> T.intercalate ", " lost)
      pure (ItemNote linkId text rel : nested)
    "group" -> pure <$> readGroup ctx v linkId text rel
    _ -> readQuestion ctx v linkId ty text rel

readGroup :: Ctx -> Value -> Text -> I18n -> Maybe Expr -> Conv Item
readGroup ctx v linkId text rel = do
  let ctrl = itemControl v
      repeats = keyBool "repeats" v == Just True
      children = keyArr "item" v
      -- A matrix's help text is its first child, as the writer puts it.
      (hints, rowItems) = case children of
        (c : more) | isHintItem c, hintIsPlain c -> ([c], more)
        _ -> ([], children)
      rowKeys = ["linkId", "text", "_text", "type", "required", "repeats", "answerOption", "answerValueSet"]
      -- Open-choice rows carry free text a matrix cannot hold.
      isRow i = keyTextOr "" "type" i `elem` ["choice", "coding"] && all (`elem` rowKeys) (objKeys i) && M.null (underscoreRest "text" i)
      rowId i = let l = keyTextOr "" "linkId" i in fromMaybe l (listToMaybe (mapMaybe (`T.stripPrefix` l) [linkId <> "_", linkId <> ".", linkId <> "/"]))
      attrsWithout consumed =
        let kept = [e | e <- extensions v, not (extUrl e == extItemControl && consumed), extUrl e /= extEnableExpr]
         in keepFields ["linkId", "text", "_text", "type", "repeats", "enableWhen", "enableBehavior", "item", "extension"] v
              <> M.fromList [("fhir:extension", toJSON kept) | not (null kept)]
              <> underscoreRest "text" v
              <> keepExpr "fhir:enableWhenExpression" (extNamed extEnableExpr v >>= key "valueExpression")
      -- What a row says beyond its label: options as they are written,
      -- their source, and whether an answer is required.
      rowShape i = (key "answerOption" i, keyText "answerValueSet" i, keyBool "required" i == Just True)
  rows <- forM rowItems $ \i -> (,) i <$> readOptions ctx i (keyTextOr "" "linkId" i)
  case rows of
    ((r0, cols) : _)
      | ctrl `elem` [Just "table", Just "htable", Just "gtable"]
      , not repeats
      , all (isRow . fst) rows
      , all ((== cols) . snd) rows
      -- Rows that differ in anything but their label (one required and one
      -- not, options with different extras) are questions of their own.
      , all ((== rowShape r0) . rowShape . fst) rows
      , let multis = map (\(i, _) -> keyBool "repeats" i == Just True) rows
      , and multis || not (or multis) ->
          pure . ItemQuestion $
            (question linkId "" (QMatrix (if or multis then Multiple else Single) [Choice (rowId i) (fromMaybe (plain "") (i18nAt "text" i)) False False | (i, _) <- rows] cols))
              { qLabel = text
              , qHint = listToMaybe hints >>= i18nAt "text"
              , qRequired = keyBool "required" r0 == Just True
              , qRelevant = rel
              , qAttrs =
                  attrsWithout True
                    <> M.fromList [("fhir:itemControl", String c) | Just c <- [ctrl], c /= "table"]
                    <> optionAttrs (keyArr "answerOption" r0)
                    <> vsAttrs ctx r0
                    <> foldMap (hintAttrs linkId) hints
                    <> M.fromList [("fhir:answerOption", raw) | Just raw <- [key "answerOption" r0], richOptions True (keyArr "answerOption" r0)]
                    <> M.fromList [("fhir:answerValueSet", String vs) | Just vs <- [keyText "answerValueSet" r0]]
                    <> M.fromList [("fhir:rowIds", toJSON ids) | let ids = [keyTextOr "" "linkId" i | (i, _) <- rows], ids /= [linkId <> "_" <> rowId i | (i, _) <- rows]]
              }
    _ -> do
      items <- concat <$> mapM (readItem ctx) children
      let page = ctrl == Just "page" && not repeats
      pure . ItemGroup $
        Group
          { groupId = linkId
          , groupKind = if repeats then Repeat else if page then Page else Section
          , groupTitle = i18nAt "text" v
          , groupRelevant = rel
          , groupItems = items
          , groupAttrs = attrsWithout page
          }

-- | A display child that is a question's help text.
isHintItem :: Value -> Bool
isHintItem c = keyText "type" c == Just "display" && itemControl c `elem` [Nothing, Just "help", Just "flyover", Just "prompt"] && null (keyArr "item" c)

-- | A help text item that is only its text, id and control.
hintIsPlain :: Value -> Bool
hintIsPlain c =
  all (`elem` ["linkId", "text", "_text", "type", "extension"]) (objKeys c)
    && all ((== extItemControl) . extUrl) (extensions c)
    && M.null (underscoreRest "text" c)

-- | The help text item's own id and control, when the writer would not
-- give it the same ones.
hintAttrs :: Text -> Value -> Attrs
hintAttrs linkId c =
  M.fromList $
    [("fhir:hintId", String l) | Just l <- [keyText "linkId" c], l /= linkId <> "_hint"]
      ++ [("fhir:hintControl", String k) | Just k <- [itemControl c]]

objKeys :: Value -> [Text]
objKeys (Object o) = map K.toText (KM.keys o)
objKeys _ = []

-- | Coding system and answer kind shared by all options, kept so a FHIR
-- round trip writes them back.
optionAttrs :: [Value] -> Attrs
optionAttrs opts =
  M.fromList $
    [("fhir:system", String s) | [s] <- [nub [s | o <- opts, Just c <- [key "valueCoding" o], Just s <- [keyText "system" c]]]]
      ++ [("fhir:answerType", String k) | [k] <- [nub (mapMaybe (fmap fst . valueX "value") opts)], k `elem` ["String", "Integer"]]

-- | Whether writing the options back from the IR would lose something:
-- an extension (an ordinal value, a prefix), a coding's version, codes
-- from several systems, answers that are dates or references, or (for a
-- matrix row, or several codes with spaces) the initial selection.
richOptions :: Bool -> [Value] -> Bool
richOptions isRow opts =
  any rich opts
    || length (nub [key "valueCoding" o >>= keyText "system" | o <- opts]) > 1
    || length (nub (map (fmap fst . valueX "value") opts)) > 1
    || (if isRow then not (null selected) else length selected > 1 && any (T.any isSpace) selected)
  where
    rich o =
      not (all (`elem` ["valueCoding", "valueString", "_valueString", "valueInteger", "extension", "initialSelected"]) (objKeys o))
        || any ((/= extExclusive) . extUrl) (extensions o)
        || maybe False (not . all (`elem` ["code", "display", "_display", "system"]) . objKeys) (key "valueCoding" o)
    selected = [fromMaybe "" (answerText c) | o <- opts, keyBool "initialSelected" o == Just True, Just (_, c) <- [valueX "value" o]]

optionChoice :: Value -> Choice
optionChoice o =
  let excl = isTrue (extNamed extExclusive o)
   in case valueX "value" o of
        Just ("Coding", c) ->
          let code = keyTextOr "" "code" c
           in Choice code (fromMaybe (plain code) (i18nAt "display" c)) False excl
        Just (k, x) ->
          let t = fromMaybe "" (asText x)
           in Choice t (fromMaybe (plain t) (i18nAt ("value" <> k) o)) False excl
        Nothing -> choice "" ""

readOptions :: Ctx -> Value -> Text -> Conv [Choice]
readOptions ctx v linkId = case keyArr "answerOption" v of
  [] -> case keyText "answerValueSet" v of
    Just vs | Just cs <- M.lookup vs (cValueSets ctx) -> pure cs
    Just vs -> [] <$ warn linkId ("answer value set " <> vs <> " not resolved")
    Nothing -> pure []
  os -> pure (map optionChoice os)

readQuestion :: Ctx -> Value -> Text -> Text -> I18n -> Maybe Expr -> Conv [Item]
readQuestion ctx v linkId ty text rel = do
  let ctrl = itemControl v
      children = keyArr "item" v
      childCtrl = itemControl
      isEnd e c = keyText "type" c == Just "display" && childCtrl c == Just e
      (hints, rest0) = partition isHintItem children
      (lowers, rest1) = partition (isEnd "lower") rest0
      (uppers, rest) = partition (isEnd "upper") rest1
      hint = listToMaybe hints >>= i18nAt "text"
      numExt u = extNamed u v >>= valueX "value" >>= \(_, x) -> case x of
        Number n -> Just (either id fromIntegral (floatingOrInteger n :: Either Double Integer))
        _ -> Nothing
      minV = numExt extMin
      maxV = numExt extMax
      calc = extNamed extCalc v >>= key "valueExpression"
      hidden = isTrue (extNamed extHidden v)
      isScale = ty == "integer" && ctrl == Just "slider" && isJust minV && isJust maxV
      open = ty == "open-choice" || keyText "answerConstraint" v `elem` [Just "optionsOrString", Just "optionsOrType"]
      openLabel = fromMaybe (plain "Other") (extNamed extOpenLabel v >>= i18nAt "valueString")
      sel = if keyBool "repeats" v == Just True then Multiple else Single
      style = if ctrl `elem` [Just "drop-down", Just "autocomplete", Just "lookup"] then Dropdown else Radio
      consumedCtrl = case ty of
        _ | isScale -> ctrl == Just "slider"
        -- Only the control the writer emits is consumed; others are kept
        -- as they are and still set the style.
        _ | ty `elem` choiceTypes -> ctrl == Just "drop-down"
        _ -> False
      consumedUrls =
        [extHidden, extCalc, extOpenLabel, extEnableExpr]
          ++ [extMin | isScale || isJust minV]
          ++ [extMax | isScale || isJust maxV]
          ++ [extStep | isScale]
          ++ [extItemControl | consumedCtrl]
          ++ [extConstraint | isJust constraintE]
      constraintE = extNamed extConstraint v
      -- A text answer's maxLength is its upper bound.
      maxLen = if ty `elem` ["string", "text", "url"] && not (isJust maxV) then keyNum "maxLength" v else Nothing
      keptExts = [e | e <- extensions v, extUrl e `notElem` consumedUrls]
  when (length [e | e <- extensions v, extUrl e == extConstraint] > 1) $ warn linkId "only the first target constraint kept"
  when (length hints > 1) $ warn linkId "only the first help text kept as the hint"
  unless (all hintIsPlain (take 1 hints)) $ warn linkId "help text item's other fields dropped"
  opts <- readOptions ctx v linkId
  qt <- case calc of
    Just e -> pure (QCalculate (ERaw "fhirpath" (keyTextOr "" "expression" e)))
    Nothing
      | hidden -> pure QHidden
      | otherwise -> pure $ case ty of
          "boolean" -> QBoolean
          "decimal" -> QNumber Decimal
          "integer"
            | isScale -> QScale (maybe 0 round minV) (maybe 10 round maxV) (listToMaybe lowers >>= i18nAt "text") (listToMaybe uppers >>= i18nAt "text")
            | otherwise -> QNumber Integer
          "date" -> QDate Date
          "dateTime" -> QDate DateTime
          "time" -> QDate Time
          "string" -> QText ShortText
          "text" -> QText LongText
          "url" -> QText Url
          "attachment" -> QFile
          t
            | t `elem` choiceTypes -> QChoice sel style (opts ++ [Choice "other" openLabel True False | open])
            | otherwise -> QOther t
  let asChoice = case qt of
        QChoice {} -> True
        _ -> False
      rawOptions = keyArr "answerOption" v
      keepRaw = not asChoice || richOptions False rawOptions
  unless (asChoice || null rawOptions) $ warn linkId ("answer options kept only for FHIR; the item is read as " <> (if isJust calc then "a calculation" else if hidden then "a hidden value" else ty))
  let lowUp = case qt of
        QScale {} -> []
        _ -> lowers ++ uppers
  nested <- concat <$> mapM (readItem ctx) (lowUp ++ rest)
  unless (null nested) $ warn linkId "items nested under a question moved after it"
  let validation = case qt of
        QScale {} -> Nothing
        _
          | isJust minV || isJust maxV || isJust maxLen || isJust constraintE ->
              Just
                Validation
                  { valMin = minV
                  , valMax = maxV `orElse` maxLen
                  , valExpr = ERaw "fhirpath" <$> (constraintE >>= fst . constraintParts)
                  , valMessage = constraintE >>= snd . constraintParts
                  }
          | otherwise -> Nothing
      selected = [t | asChoice, o <- rawOptions, keyBool "initialSelected" o == Just True, Just (_, c) <- [valueX "value" o], Just t <- [answerText c]]
      initials = keyArr "initial" v
      -- One selected option is the default as it is; several are joined
      -- with spaces, unless a code holds a space (then the options are
      -- kept as written, see 'richOptions').
      initial = case selected of
        [t] -> Just t
        (_ : _) -> if any (T.any isSpace) selected then Nothing else Just (T.unwords selected)
        [] -> case initials of
          [i] -> valueX "value" i >>= answerText . snd
          _ -> Nothing
      severalInitial = null selected && length initials > 1
      isChoice = ty `elem` choiceTypes
      usedMaxLen = isJust maxLen && case qt of QScale {} -> False; _ -> True
      attrs =
        keepFields (["linkId", "text", "_text", "type", "required", "enableWhen", "enableBehavior", "item", "extension", "answerConstraint"] ++ ["initial" | not severalInitial] ++ ["answerOption" | not keepRaw] ++ ["repeats" | isChoice] ++ ["maxLength" | usedMaxLen]) v
          <> M.fromList [("fhir:extension", toJSON keptExts) | not (null keptExts)]
          <> M.fromList [("fhir:type", String ty) | isJust calc || hidden, ty /= "string"]
          <> M.fromList [("fhir:visible", Bool True) | isJust calc, not hidden]
          <> (if isChoice then optionAttrs (keyArr "answerOption" v) <> vsAttrs ctx v else M.empty)
          <> foldMap (hintAttrs linkId) (take 1 hints)
          <> underscoreRest "text" v
          <> keepExpr "fhir:calculatedExpression" calc
          <> keepExpr "fhir:enableWhenExpression" (extNamed extEnableExpr v >>= key "valueExpression")
          <> M.fromList [("fhir:targetConstraint", c) | Just c <- [constraintE], (Just src, human) <- [constraintParts c], c /= constraintExt linkId src (i18nDefault <$> human)]
  when severalInitial $ warn linkId "several initial answers kept only for FHIR"
  pure $
    ItemQuestion
      Question
        { qId = linkId
        , qLabel = text
        , qHint = hint
        , qType = qt
        , qRequired = keyBool "required" v == Just True
        , qRelevant = rel
        , qValidation = validation
        , qDefault = initial
        , qAttrs = attrs
        }
      : nested

answerText :: Value -> Maybe Text
answerText = \case
  Object o | Just c <- KM.lookup "code" o -> asText c
  Object o | Just r <- KM.lookup "reference" o -> asText r
  Object o | Just x <- KM.lookup "value" o -> asText x
  x -> asText x

readEnableWhen :: Ctx -> Value -> Conv (Maybe Expr)
readEnableWhen ctx v = do
  when (length [e | e <- extensions v, extUrl e == extEnableExpr] > 1) $ warn (keyTextOr "" "linkId" v) "only the first enableWhenExpression kept"
  atoms <- mapM cond (keyArr "enableWhen" v)
  let op = if keyText "enableBehavior" v == Just "any" then Or else And
      fromWhen = case catMaybes atoms of
        [] -> Nothing
        (a : as) -> Just (foldl (EBin op) a as)
      fromExpr = (\e -> ERaw "fhirpath" (keyTextOr "" "expression" e)) <$> (extNamed extEnableExpr v >>= key "valueExpression")
  pure $ case (fromWhen, fromExpr) of
    (Just a, Just b) -> Just (EBin And a b)
    (a, b) -> a `orElse` b
  where
    at = keyTextOr "" "linkId" v
    cond c = do
      let q = keyTextOr "" "question" c
          op = keyTextOr "" "operator" c
          ans = valueX "answer" c
          lit = case ans of
            Just ("Boolean", Bool b) -> Just (LBool b)
            Just ("Integer", Number n) -> Just (LNum (realToFrac n))
            Just ("Decimal", Number n) -> Just (LNum (realToFrac n))
            Just ("Quantity", x) -> LNum <$> keyNum "value" x
            Just (_, x) -> LStr <$> answerText x
            Nothing -> Nothing
          multi = q `S.member` cMulti ctx
      when (fmap fst ans == Just "Quantity") $ warn at "enableWhen quantity unit dropped"
      case (op, lit) of
        ("exists", Just (LBool b)) -> pure (Just (if b then EAnswered q else ENot (EAnswered q)))
        ("=", Just (LStr s)) | multi -> pure (Just (ESelected q s))
        ("!=", Just (LStr s)) | multi -> pure (Just (ENot (ESelected q s)))
        (_, Just l) | Just o <- lookup op cmpTable -> pure (Just (EBin o (ERef q) (ELit l)))
        _ -> Nothing <$ warn at ("enableWhen condition on " <> q <> " with operator " <> op <> " dropped")

cmpTable :: [(Text, BinOp)]
cmpTable = [("=", Eq), ("!=", Neq), (">", Gt), ("<", Lt), (">=", Ge), ("<=", Le)]

------------------------------------------------------------------------------
-- Writing

writeFHIR :: Form -> Conv BL.ByteString
writeFHIR s@(Form meta items) = do
  let sets = case M.lookup "fhir:contained" (metaAttrs meta) of
        Just (Array a) -> valueSetChoices (V.toList a)
        _ -> M.empty
      qs = Env (M.fromList [(qId q, q) | q <- questions s]) sets
  body <- concat <$> mapM (writeItem qs) items
  let attrs = metaAttrs meta
  pure . encodePretty . Object . KM.fromList $
    [("resourceType", "Questionnaire"), ("item", toJSON body)]
      ++ [("status", "draft") | not (M.member "fhir:status" attrs)]
      ++ [("name", String i) | Just i <- [metaId meta], not (M.member "fhir:name" attrs || M.member "fhir:id" attrs)]
      ++ maybe [] (i18nFields "title") (metaTitle meta)
      ++ maybe [] (i18nFields "description") (metaDescription meta)
      ++ [("language", String l) | (l : _) <- [metaLanguages meta]]
      ++ [("extension", e) | Just e <- [M.lookup "fhir:extension" attrs]]
      ++ attrFields attrs

-- | Questions by id, and the contained value sets by @#id@.
data Env = Env
  { eQuestions :: M.Map Text Question
  , eSets :: M.Map Text [Choice]
  }

type Qs = Env

writeItem :: Qs -> Item -> Conv [Value]
writeItem qs = \case
  ItemRaw (Format "fhir") v -> pure [v]
  ItemRaw (Format f) _ -> [] <$ warn "fhir" ("raw " <> f <> " content dropped")
  ItemNote i t rel -> do
    (ew, ex) <- enableWhen qs M.empty i rel
    pure [itemObj [("linkId", String i), ("type", "display")] (i18nFields "text" t ++ ew) ex M.empty]
  ItemGroup g -> do
    (ew, ex) <- enableWhen qs (groupAttrs g) (groupId g) (groupRelevant g)
    kids <- concat <$> mapM (writeItem qs) (groupItems g)
    -- SDC renderers read itemControl "page"; the R4-core validator rejects the
    -- code only because it predates it. Dropping it would lose page breaks.
    let ctl = [controlExt "page" | groupKind g == Page]
    pure
      [ itemObj
          ([("linkId", String (groupId g)), ("type", "group")] ++ [("item", toJSON kids) | not (null kids)] ++ [("repeats", Bool True) | groupKind g == Repeat])
          (maybe [] (i18nFields "text") (groupTitle g) ++ ew)
          (ctl ++ ex)
          (groupAttrs g)
      ]
  ItemQuestion q -> pure <$> writeQuestion qs q

-- | An item object: fixed fields, IR-derived fields and extensions, then
-- preserved attributes (which never override the others).
itemObj :: [(K.Key, Value)] -> [(K.Key, Value)] -> [Value] -> Attrs -> Value
itemObj fixed fields exts attrs =
  let kept = case M.lookup "fhir:extension" attrs of
        Just (Array a) -> V.toList a
        _ -> []
      allExts = exts ++ kept
      -- Translations of the text, with whatever else its @_text@ held.
      underscore = case (lookup "_text" fields, M.lookup "fhir:_text" attrs) of
        (Just g@(Object go), Just k@(Object ko)) -> [("_text", Object (KM.insert "extension" (toJSON (extensions g ++ extensions k)) (KM.union go ko)))]
        (Nothing, Just k) | isJust (lookup "text" fields) -> [("_text", k)]
        _ -> []
   in Object . KM.fromList $
        attrFields attrs ++ fixed ++ fields ++ underscore ++ [("extension", toJSON allExts) | not (null allExts)]

-- | @enableWhen@ fields, or an SDC @enableWhenExpression@ extension for
-- raw FHIRPath.
enableWhen :: Qs -> Attrs -> Text -> Maybe Expr -> Conv ([(K.Key, Value)], [Value])
enableWhen _ _ _ Nothing = pure ([], [])
enableWhen qs attrs at (Just e) = case e of
  ERaw "fhirpath" src -> pure ([], [object ["url" .= extEnableExpr, "valueExpression" .= exprObj attrs "fhir:enableWhenExpression" src]])
  -- Both enableWhen and an expression, as the reader combines them.
  EBin And x r@(ERaw "fhirpath" _) | isJust (flatten x :: Maybe (Maybe Text, [Value])) -> do
    (ew, _) <- enableWhen qs attrs at (Just x)
    (_, ex) <- enableWhen qs attrs at (Just r)
    pure (ew, ex)
  _ -> case flatten e of
    Just (beh, conds) -> pure ([("enableWhen", toJSON conds)] ++ [("enableBehavior", String b) | Just b <- [beh]], [])
    Nothing -> ([], []) <$ warn at "logic dropped, fhir enableWhen cannot express nested and/or or this condition"
  where
    flatten x = case x of
      EBin And _ _ -> (,) (Just "all") <$> mapM (atom qs) (chain And x)
      EBin Or _ _ -> (,) (Just "any") <$> mapM (atom qs) (chain Or x)
      _ -> (,) Nothing . pure <$> atom qs x
    chain op x = case x of
      EBin o a b | o == op -> chain op a ++ chain op b
      _ -> [x]

atom :: Qs -> Expr -> Maybe Value
atom env e = case e of
  EAnswered q -> Just (cond q "exists" ("answerBoolean", Bool True))
  ENot (EAnswered q) -> Just (cond q "exists" ("answerBoolean", Bool False))
  ESelected q v -> cond q "=" <$> answer q (LStr v)
  ENot (ESelected q v) -> cond q "!=" <$> answer q (LStr v)
  EBin o (ERef q) (ELit l) | Just op <- opText o -> cond q op <$> answer q l
  EBin o (ELit l) (ERef q) | Just op <- opText (flipOp o) -> cond q op <$> answer q l
  ENot x -> pushNot' x >>= atom env
  _ -> Nothing
  where
    qs = eQuestions env
    cond :: Text -> Text -> (Text, Value) -> Value
    cond q op (k, v) = object ["question" .= q, "operator" .= op, K.fromText k .= v]
    opText o = lookup o [(b, a) | (a, b) <- cmpTable]
    flipOp o = fromMaybe o (lookup o [(Lt, Gt), (Gt, Lt), (Le, Ge), (Ge, Le)])
    pushNot' x = case x of
      EBin o a b -> (\o' -> EBin o' a b) <$> lookup o [(Eq, Neq), (Neq, Eq), (Lt, Ge), (Ge, Lt), (Gt, Le), (Le, Gt)]
      ENot y -> Just y
      _ -> Nothing
    -- The answer's type follows the question it tests.
    answer q l = case (qType <$> M.lookup q qs, l) of
      (Just (QChoice {}), _) -> Just (choiceAnswer (M.lookup q qs) (litText l))
      (Just (QRanking _), _) -> Just (choiceAnswer (M.lookup q qs) (litText l))
      (Just QBoolean, LBool b) -> Just ("answerBoolean", Bool b)
      (Just QBoolean, LStr t) | T.toLower t `elem` ["true", "yes", "1"] -> Just ("answerBoolean", Bool True)
      (Just QBoolean, LStr t) | T.toLower t `elem` ["false", "no", "0"] -> Just ("answerBoolean", Bool False)
      (Just (QNumber Decimal), LNum n) -> Just ("answerDecimal", numValue n)
      (Just (QDate Date), LStr t) -> Just ("answerDate", String t)
      (Just (QDate DateTime), LStr t) -> Just ("answerDateTime", String t)
      (Just (QDate Time), LStr t) -> Just ("answerTime", String t)
      (_, LNum n) | isIntegral n -> Just ("answerInteger", toJSON (round n :: Integer))
      (_, LNum n) -> Just ("answerDecimal", numValue n)
      (_, LBool b) -> Just ("answerBoolean", Bool b)
      (_, LStr t) -> Just ("answerString", String t)
    litText = \case
      LStr t -> t
      LNum n | isIntegral n -> T.pack (show (round n :: Integer))
      LNum n -> T.pack (show n)
      LBool b -> if b then "true" else "false"

-- | A choice answer, typed the way the question's options are.
choiceAnswer :: Maybe Question -> Text -> (Text, Value)
choiceAnswer mq t = case M.lookup "fhir:answerType" attrs of
  Just (String "String") -> ("answerString", String t)
  Just (String "Integer") | [(n, "")] <- reads (T.unpack t) -> ("answerInteger", toJSON (n :: Integer))
  _ -> ("answerCoding", codingObj attrs t Nothing)
  where
    attrs = maybe M.empty qAttrs mq

codingObj :: Attrs -> Text -> Maybe I18n -> Value
codingObj attrs code display =
  Object . KM.fromList $
    [("system", sys) | Just sys <- [M.lookup "fhir:system" attrs]]
      ++ [("code", String code)]
      ++ maybe [] (i18nFields "display") display

writeQuestion :: Qs -> Question -> Conv Value
writeQuestion qs q = do
  (ew, ex) <- enableWhen qs (qAttrs q) (qId q) (qRelevant q)
  let note = warn (qId q)
      attrs0 = qAttrs q
      str k = case M.lookup k attrs0 of
        Just (String t) -> Just t
        _ -> Nothing
      attrType = fromMaybe "string" (str "fhir:type")
      hintItem = [displayChild (fromMaybe (qId q <> "_hint") (str "fhir:hintId")) (str "fhir:hintControl") h | Just h <- [qHint q]]
      -- Options come from a contained value set while it still lists
      -- exactly these choices (or none could be listed); then the
      -- reference is written instead of the options.
      setRef cs = case str "fhir:answerValueSet" of
        Just ref | null cs || M.lookup ref (eSets qs) == Just cs -> Just ref
        _ -> Nothing
      -- Options read with more than the IR holds are written as they
      -- were while they still name the same choices.
      optionList cs = case M.lookup "fhir:answerOption" attrs0 of
        Just (Array a) | map optionChoice (V.toList a) == cs -> pure (V.toList a)
        _ -> options cs
      choiceOptions cs = case setRef cs of
        Just _ -> pure []
        Nothing -> (\opts -> [("answerOption", toJSON opts) | not (null opts)]) <$> optionList cs
      isChoiceType = case qType q of
        QChoice {} -> True
        QMatrix {} -> True
        QRanking _ -> True
        _ -> False
      choicesHere = case qType q of
        QChoice _ _ cs -> filter (not . choiceOther) cs
        QMatrix _ _ cols -> cols
        QRanking cs -> cs
        _ -> []
      -- The item's own fields never carry what the writer decides here.
      attrs
        | not isChoiceType = attrs0
        | otherwise =
            (case (qType q, setRef choicesHere) of
               (QChoice {}, Just _) -> id
               (QRanking _, Just _) -> id
               _ -> M.delete "fhir:answerValueSet")
              (M.delete "fhir:answerOption" attrs0)
  (ty, fields, exts, children) <- case qType q of
    QText k -> do
      when (k `elem` [Email, Phone]) $ note "email/phone input written as a plain string"
      pure (case k of LongText -> "text"; Url -> "url"; _ -> "string", [], [], [])
    QNumber Integer -> pure ("integer", [], [], [])
    QNumber Decimal -> pure ("decimal", [], [], [])
    QDate k -> pure (case k of Date -> "date"; Time -> "time"; DateTime -> "dateTime", [], [], [])
    QBoolean -> pure ("boolean", [], [], [])
    QFile -> pure ("attachment", [], [], [])
    QGeo -> ("string", [], [], []) <$ note "geopoint written as a plain string"
    QChoice sel style cs -> do
      let (others, regular) = partition choiceOther cs
      when (length others > 1) $ note "only one other choice kept"
      unless (all ((== "other") . choiceValue) others) $ note "other choice value dropped"
      when (style == Buttons) $ note "button style written as a default choice control"
      opts <- choiceOptions regular
      let keptControl = case M.lookup "fhir:extension" attrs of
            Just (Array a) -> any ((== extItemControl) . extUrl) a
            _ -> False
          ctl = [controlExt "drop-down" | style == Dropdown, not keptControl]
          openLbl = [object (("url", String extOpenLabel) : i18nFields "valueString" (choiceLabel o)) | o <- take 1 others, choiceLabel o /= plain "Other"]
      pure
        ( if null others then "choice" else "open-choice"
        , opts ++ [("repeats", Bool True) | sel == Multiple]
        , ctl ++ openLbl
        , []
        )
    QRanking cs -> do
      note "ranking written as a multi-select choice"
      opts <- choiceOptions cs
      pure ("choice", opts ++ [("repeats", Bool True)], [], [])
    QScale lo hi l h ->
      pure
        ( "integer"
        , []
        , [ controlExt "slider"
          , object ["url" .= extMin, "valueInteger" .= lo]
          , object ["url" .= extMax, "valueInteger" .= hi]
          , object ["url" .= extStep, "valueInteger" .= (1 :: Int)]
          ]
        , [displayChild (qId q <> "_lower") (Just "lower") t | Just t <- [l]] ++ [displayChild (qId q <> "_upper") (Just "upper") t | Just t <- [h]]
        )
    QMatrix sel rows cols -> do
      opts <- choiceOptions cols
      let ctl = case M.lookup "fhir:itemControl" attrs of
            Just (String c) -> c
            _ -> "table"
          -- Row ids as they were read, else <matrix>_<row code>.
          rowIds = case M.lookup "fhir:rowIds" attrs of
            Just (Array a) | ids <- [t | String t <- V.toList a], length ids == length rows -> ids
            _ -> [qId q <> "_" <> choiceValue c | c <- rows]
          row (i, c) =
            Object . KM.fromList $
              [("linkId", String i), ("type", "choice")]
                ++ opts
                ++ [("answerValueSet", String ref) | Just ref <- [setRef cols]]
                ++ i18nFields "text" (choiceLabel c)
                ++ [("required", Bool True) | qRequired q]
                ++ [("repeats", Bool True) | sel == Multiple]
      when (null rows) $ note "matrix without rows written as an empty group"
      -- The rows are the group's items, after its help text.
      pure ("group", [], [controlExt ctl], map row (zip rowIds rows))
    QHidden -> pure (attrType, [], [boolExt extHidden], [])
    QCalculate e -> do
      calc <- case e of
        ERaw "fhirpath" src -> pure [object ["url" .= extCalc, "valueExpression" .= exprObj attrs "fhir:calculatedExpression" src]]
        ERaw d _ -> [] <$ note ("calculation in " <> d <> " syntax dropped")
        _ -> [] <$ note "calculation dropped, fhir needs FHIRPath"
      pure (attrType, [], calc ++ [boolExt extHidden | M.lookup "fhir:visible" attrs /= Just (Bool True)], [])
    QOther t
      | t `elem` fhirTypes -> pure (t, [], [], [])
      | otherwise -> ("string", [], [], []) <$ note ("type " <> t <> " written as a string")
  (valFields, valExts) <- validation ty
  initial <- defaultFields ty
  let isMatrix = case qType q of QMatrix {} -> True; _ -> False
      kids = hintItem ++ children
  pure $
    itemObj
      ([("linkId", String (qId q)), ("type", String ty)] ++ [("required", Bool True) | qRequired q, not isMatrix])
      (i18nFields "text" (qLabel q) ++ ew ++ fields ++ valFields ++ initial ++ [("item", toJSON kids) | not (null kids)])
      (exts ++ valExts ++ ex)
      attrs
  where
    options cs = pure (map optionObj cs)
    optionObj c =
      let excl = [("extension", toJSON [boolExt extExclusive]) | choiceExclusive c]
          initSel = [("initialSelected", Bool True) | isInitial (choiceValue c)]
          val = case M.lookup "fhir:answerType" (qAttrs q) of
            Just (String "String") -> ("valueString", String (choiceValue c)) : [(k, v) | (k, v) <- i18nFields "valueString" (choiceLabel c), k /= "valueString"]
            Just (String "Integer") | [(n, "")] <- reads (T.unpack (choiceValue c)) -> [("valueInteger", toJSON (n :: Integer))]
            _ -> [("valueCoding", codingObj (qAttrs q) (choiceValue c) (if choiceLabel c == plain (choiceValue c) then Nothing else Just (choiceLabel c)))]
       in Object (KM.fromList (val ++ excl ++ initSel))
    isInitial v = maybe False (v `elem`) defaultCodes
    -- A default naming options marks them initialSelected; any other
    -- default (free text on an open choice) is an initial string. A
    -- default that is one option's code is that option, spaces and all;
    -- otherwise it is a list of codes.
    optionDefault = isJust defaultCodes
    defaultCodes = case (qType q, qDefault q) of
      (QChoice _ _ cs, Just d)
        | d `elem` map choiceValue cs -> Just [d]
        | ws@(_ : _) <- T.words d, all (`elem` map choiceValue cs) ws -> Just ws
      _ -> Nothing
    displayChild i ctl t =
      Object . KM.fromList $
        [("linkId", String i), ("type", "display")] ++ i18nFields "text" t ++ [("extension", toJSON [controlExt c]) | Just c <- [ctl]]
    validation ty = case qValidation q of
      Nothing -> pure ([], [])
      Just v -> do
        let isText = ty `elem` ["string", "text", "url"]
            bound u x
              | ty == "integer" && isIntegral x = Just (object ["url" .= u, "valueInteger" .= (round x :: Integer)])
              | ty `elem` ["integer", "decimal", "quantity"] = Just (object ["url" .= u, "valueDecimal" .= numValue x])
              | otherwise = Nothing
            mins = [e | Just x <- [valMin v], Just e <- [bound extMin x]]
            maxs = [e | Just x <- [valMax v], Just e <- [bound extMax x]]
            maxLen = [("maxLength", toJSON (round x :: Integer)) | isText, Just x <- [valMax v]]
        when (isJust (valMin v) && null mins) $ warn (qId q) "minimum dropped"
        when (isJust (valMax v) && null maxs && null maxLen) $ warn (qId q) "maximum dropped"
        con <- case valExpr v of
          Just (ERaw "fhirpath" src) -> pure $ case M.lookup "fhir:targetConstraint" (qAttrs q) of
            -- A constraint read with its own key, severity or translated
            -- message is written as it was while it says the same thing.
            Just c | constraintParts c == (Just src, valMessage v) -> [c]
            _ -> [constraintExt (qId q) src (i18nDefault <$> valMessage v)]
          Just _ -> [] <$ warn (qId q) "validation expression dropped, fhir needs FHIRPath"
          Nothing -> [] <$ when (isJust (valMessage v)) (warn (qId q) "validation message dropped")
        pure (maxLen, mins ++ maxs ++ con)
    defaultFields ty = case (qDefault q, qType q) of
      (Nothing, _) -> pure []
      -- Options that are only referenced cannot be marked selected.
      (Just _, QChoice _ _ cs)
        | optionDefault
        , Just (String ref) <- M.lookup "fhir:answerValueSet" (qAttrs q)
        , let regular = filter (not . choiceOther) cs
        , null regular || M.lookup ref (eSets qs) == Just regular ->
            pure [("initial", toJSON [object ["valueCoding" .= codingObj (qAttrs q) c Nothing] | c <- fromMaybe [] defaultCodes])]
      (Just _, QChoice {}) | optionDefault -> pure []
      (Just d, QChoice {}) -> pure [("initial", toJSON [object ["valueString" .= d]])]
      (Just d, _) -> case initialValue ty d of
        Just kv -> pure [("initial", toJSON [Object (KM.fromList [kv])])]
        Nothing -> [] <$ warn (qId q) "default value dropped"
    initialValue ty d = case ty of
      "boolean" -> Just ("valueBoolean", Bool (T.toLower d `elem` ["true", "yes", "1"]))
      "integer" | [(n, "")] <- reads (T.unpack d) -> Just ("valueInteger", toJSON (n :: Integer))
      "decimal" | [(n, "")] <- reads (T.unpack d) -> Just ("valueDecimal", numValue n)
      "date" -> Just ("valueDate", String d)
      "dateTime" -> Just ("valueDateTime", String d)
      "time" -> Just ("valueTime", String d)
      t | t `elem` ["string", "text"] -> Just ("valueString", String d)
      "url" -> Just ("valueUri", String d)
      _ -> Nothing

fhirTypes :: [Text]
fhirTypes = ["quantity", "reference", "question", "coding"]
