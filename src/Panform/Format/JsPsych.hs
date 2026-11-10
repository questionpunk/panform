-- | jsPsych timelines as JSON, with plugin names as strings
-- (@"type": "jsPsychSurveyMultiChoice"@) for a loader to map to the plugin
-- objects. The writer emits the classic @survey-multi-choice@,
-- @-multi-select@, @-text@ and @-likert@ trials when they carry the survey
-- in full, and otherwise one @jsPsychSurvey@ trial (\@jspsych/plugin-survey)
-- holding the SurveyJS model. The reader takes both.
module Panform.Format.JsPsych (readJsPsych, writeJsPsych) where

import Control.Monad (forM, unless)
import Data.Aeson (Value (..), eitherDecode, encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, toLower)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Format.SurveyJS (readSurveyJS, writeSurveyJS)
import Panform.Format.TextUtil
import Panform.Json

------------------------------------------------------------------------------
-- Reading

data Trial
  = FromModel Form
  -- ^ A @jsPsychSurvey@ trial's SurveyJS model.
  | Screen Text (Maybe Text) [Item]
  -- ^ A classic trial: page id, page title, questions.
  | Skipped

readJsPsych :: BL.ByteString -> Conv Form
readJsPsych bs = do
  v <- eitherDecode bs `orFail` "jsPsych timeline JSON"
  trials <- forM (zip [1 :: Int ..] (flatten v)) (uncurry readTrial)
  let top = key "data" v
      models = [m | FromModel m <- trials]
      meta = case (top >>= keyText "survey_title", models) of
        -- Mark a survey-plugin source so the writer keeps that mode.
        (Nothing, m : _) -> (formMeta m) {metaAttrs = M.insert "jspsych:plugin" (Bool True) (metaAttrs (formMeta m))}
        (t, _) -> emptyMeta {metaTitle = plain <$> t, metaDescription = plain <$> (top >>= keyText "survey_description")}
  pure (Form meta (concatMap toItems (groupBy samePage trials)))
  where
    samePage (Screen a _ _) (Screen b _ _) = a == b
    samePage _ _ = False
    toItems = \case
      ts@(Screen p _ _ : _) ->
        let title = case [t | Screen _ (Just t) _ <- ts] of
              (t : _) -> Just (plain t)
              [] -> Nothing
         in [ItemGroup (Group p Page title Nothing (concat [is | Screen _ _ is <- ts]) M.empty)]
      ts -> concat [formItems m | FromModel m <- ts]

-- | The trials of a timeline, depth first; nested timeline nodes are
-- spliced in.
flatten :: Value -> [Value]
flatten v = case v of
  Array a -> concatMap flatten (toList a)
  Object _
    | Just t <- key "timeline" v -> flatten t
    | otherwise -> [v]
  _ -> []

-- | @jsPsychSurveyMultiChoice@, @survey-multi-choice@ and
-- @SurveyMultiChoice@ all name the same plugin.
pluginName :: Text -> Text
pluginName t =
  let n = T.filter isAlphaNum (T.map toLower t)
   in fromMaybe n (T.stripPrefix "jspsych" n)

readTrial :: Int -> Value -> Conv Trial
readTrial n t = case pluginName (keyTextOr "" "type" t) of
  "survey" -> case key "survey_json" t of
    Just (String s) -> FromModel <$> readSurveyJS (BL.fromStrict (TE.encodeUtf8 s))
    Just m -> FromModel <$> readSurveyJS (encode m)
    Nothing -> Skipped <$ warn at "survey trial without survey_json skipped"
  "surveymultichoice" -> screen (map (choiceQ Single) qs)
  "surveymultiselect" -> screen (map (choiceQ Multiple) qs)
  "surveytext" -> screen (map textQ qs)
  "surveylikert"
    | Just m <- dat >>= keyText "matrix" ->
        let rows = [choice (qName i q) (keyTextOr "" "prompt" q) | (i, q) <- qs]
            cols = case qs of
              ((_, q) : _) -> positional (keyArr "labels" q)
              [] -> []
         in pure . Screen page pageTitle . pure . ItemQuestion $
              (question m (keyTextOr "" "preamble" t) (QMatrix Single rows cols)) {qRequired = any ((== Just True) . keyBool "required" . snd) qs}
    | otherwise -> screen [(question (qName i q) (keyTextOr "" "prompt" q) (QChoice Single Buttons (positional (keyArr "labels" q)))) {qRequired = req q} | (i, q) <- qs]
  -- Experiment trials: a button press is a single choice (the response
  -- is the button's index); a keyboard trial is text on screen.
  p | p `elem` ["htmlbuttonresponse", "imagebuttonresponse"] -> do
    extras
    warn at "button trial read as a single choice"
    pure (Screen page pageTitle [ItemQuestion (question trialId (keyTextOr "" "stimulus" t) (QChoice Single Buttons (positional (keyArr "choices" t))))])
  p | p `elem` ["htmlkeyboardresponse", "imagekeyboardresponse"] -> do
    extras
    warn at "keyboard trial read as display text; key responses dropped"
    pure (Screen page pageTitle [ItemNote trialId (plain (keyTextOr "" "stimulus" t)) Nothing])
  other -> Skipped <$ warn at ("trial type " <> other <> " skipped")
  where
    at = "trial " <> T.pack (show n)
    dat = key "data" t
    trialId = fromMaybe ("trial" <> T.pack (show n)) (keyText "name" t)
    extras = mapM_ (\k -> warn at (k <> " dropped")) [k | k <- ["timeline_variables", "trial_duration", "repetitions"], isJust (key k t)]
    page = fromMaybe ("trial" <> T.pack (show n)) (dat >>= keyText "page")
    pageTitle = case dat >>= keyText "page_title" of
      Just x -> Just x
      Nothing
        | isNothing (dat >>= keyText "matrix") -> keyText "preamble" t
        | otherwise -> Nothing
    qs = zip [0 :: Int ..] (keyArr "questions" t)
    screen = pure . Screen page pageTitle . map ItemQuestion
    qName i q = fromMaybe ("t" <> T.pack (show n) <> "_Q" <> T.pack (show i)) (keyText "name" q)
    req q = keyBool "required" q == Just True
    choiceQ sel (i, q) =
      let opts = [choice o o | Just o <- map asText (keyArr "options" q)]
          style = if keyBool "horizontal" q == Just True then Buttons else Radio
       in (question (qName i q) (keyTextOr "" "prompt" q) (QChoice sel style opts)) {qRequired = req q}
    textQ (i, q) =
      (question (qName i q) (keyTextOr "" "prompt" q) (QText (if maybe 1 id (keyNum "rows" q) > 1 then LongText else ShortText)))
        { qRequired = req q
        , qAttrs = M.fromList [("jspsych:" <> k, x) | k <- ["placeholder", "columns"], Just x <- [key k q]]
        }
    -- Likert responses are the label's index.
    positional ls = [choice (T.pack (show i)) l | (i, Just l) <- zip [0 :: Int ..] (map asText ls)]

------------------------------------------------------------------------------
-- Writing

writeJsPsych :: Form -> Conv BL.ByteString
writeJsPsych s
  | classicOk s = pure (writeClassic s)
  | otherwise = do
      -- Reading the trial back gives the SurveyJS reading of the form,
      -- marked as a survey-plugin source; say so unless it already was one.
      unless (M.member "jspsych:plugin" (metaAttrs (formMeta s))) $
        warn "jspsych" "written as one jsPsychSurvey trial holding a SurveyJS model; reads back as that model"
      model <- writeSurveyJS s
      v <- eitherDecode model `orFail` "SurveyJS model"
      pure . encodePretty $ obj ["timeline" .=? toJSON [obj ["type" .=? "jsPsychSurvey", "survey_json" .=? v]]]

-- | Classic trials hold the survey in full: pages of plain questions, no
-- logic, validation, hints, defaults, translations or coded options (bar
-- Likert's positional codes).
classicOk :: Form -> Bool
classicOk s@(Form meta items) =
  isNothing (metaId meta)
    && not (null (questions s))
    && not (M.member "jspsych:plugin" (metaAttrs meta))
    && metaLanguages meta `elem` [[], ["en"]]
    && null (concatMap i18nLangs (surveyTexts s))
    && all top items
  where
    top = \case
      ItemGroup g -> groupKind g == Page && isNothing (groupRelevant g) && all leaf (groupItems g)
      i -> leaf i
    leaf = \case
      ItemQuestion q ->
        isNothing (qRelevant q) && isNothing (qValidation q) && isNothing (qHint q) && isNothing (qDefault q) && okType (qType q)
      _ -> False
    okType = \case
      QText k -> k `elem` [ShortText, LongText]
      QChoice sel style cs -> style /= Dropdown && (all plainChoice cs || (sel == Single && style == Buttons && likert cs))
      QMatrix Single _ cols -> likert cols
      _ -> False

plainChoice :: Choice -> Bool
plainChoice c = choiceValue c == txt (choiceLabel c) && not (choiceOther c || choiceExclusive c)

-- | Likert responses are the label's index, so the codes must be 0, 1, ...
likert :: [Choice] -> Bool
likert cs = map choiceValue cs == [T.pack (show i) | i <- [0 .. length cs - 1]] && not (any (\c -> choiceOther c || choiceExclusive c) cs)

data Kind = KMulti | KSelect | KText | KLikert | KMatrix Text
  deriving (Eq)

writeClassic :: Form -> BL.ByteString
writeClassic (Form meta items) =
  encodePretty $
    obj
      [ if null (catMaybes info) then Nothing else "data" .=? obj info
      , "timeline" .=? toJSON (concat [pageTrials (pageId n g) (txt <$> (g >>= groupTitle)) qs | (n, (g, qs)) <- zip [1 :: Int ..] (paginate items)])
      ]
  where
    info = [("survey_title" .=?) . String . txt =<< metaTitle meta, ("survey_description" .=?) . String . txt =<< metaDescription meta]
    kind q = case qType q of
      QChoice Single _ cs | not (all plainChoice cs) -> KLikert
      QChoice Single _ _ -> KMulti
      QChoice Multiple _ _ -> KSelect
      QMatrix {} -> KMatrix (qId q)
      _ -> KText
    pageTrials p title kids =
      [ trial first run
      | (first, run) <- zip (True : repeat False) (groupBy ((==) `on` kind) [q | ItemQuestion q <- kids])
      ]
      where
        trial first run = case run of
          (q : _) | QMatrix _ rows cols <- qType q ->
            obj
              [ "type" .=? "jsPsychSurveyLikert"
              , "preamble" .=? String (txt (qLabel q))
              , "questions" .=? toJSON [obj ["prompt" .=? String (txt (choiceLabel r)), "name" .=? String (choiceValue r), "labels" .=? toJSON (map (txt . choiceLabel) cols), req q] | r <- rows]
              , dataField first [("matrix", String (qId q))]
              ]
          (q : _) | kind q == KLikert ->
            obj
              [ "type" .=? "jsPsychSurveyLikert"
              , if first then ("preamble" .=?) . String =<< title else Nothing
              , "questions" .=? toJSON [obj ["prompt" .=? String (txt (qLabel x)), "name" .=? String (qId x), "labels" .=? toJSON (map (txt . choiceLabel) (qChoices x)), req x] | x <- run]
              , dataField first []
              ]
          (q : _) ->
            obj
              [ "type" .=? String (case kind q of KMulti -> "jsPsychSurveyMultiChoice"; KSelect -> "jsPsychSurveyMultiSelect"; _ -> "jsPsychSurveyText")
              , if first then ("preamble" .=?) . String =<< title else Nothing
              , "questions" .=? toJSON (map question' run)
              , dataField first []
              ]
          [] -> obj []
        dataField first extra = "data" .=? obj ([Just (k, v) | (k, v) <- ("page", String p) : extra] ++ [("page_title" .=?) . String =<< title | first])
    qChoices q = case qType q of
      QChoice _ _ cs -> cs
      _ -> []
    req q = if qRequired q then "required" .=? Bool True else Nothing
    question' q =
      obj $
        [ "prompt" .=? String (txt (qLabel q))
        , "name" .=? String (qId q)
        , req q
        ]
          ++ case qType q of
            QChoice _ style cs -> ["options" .=? toJSON (map (txt . choiceLabel) cs), if style == Buttons then "horizontal" .=? Bool True else Nothing]
            QText k -> [if k == LongText then "rows" .=? Number 5 else Nothing] ++ [Just (K.fromText n, v) | n <- ["placeholder", "columns"], Just v <- [M.lookup ("jspsych:" <> n) (qAttrs q)]]
            _ -> []
