-- | QuestionPunk survey JSON (@.qp.json@): the survey payload the
-- QuestionPunk app's chat editor parses (@parseChatEditAIResponse@) and
-- persists, so a survey from any format can be created in the app without
-- asking a model to rebuild it. Items are a flat list with a flat
-- @itemDetails@ per answer type; sections and display logic point at items
-- by 1-based position, and choice options are addressed by index.
--
-- > {"survey_title": "Customer feedback",
-- >  "survey_items": [{"text": "Have you used it?", "answer_type": "multiple_choice",
-- >                    "internal_name": "used", "required": true,
-- >                    "itemDetails": {"answer_options": ["Yes", "No"], "multiple_selection": false}}, ...],
-- >  "survey_sections": [{"title": "About you", "item_indices": [1, 2]}],
-- >  "survey_display_logic": [{"target_question": 2, "container_condition": "or",
-- >    "groups": [{"source_question": 1, "condition": "or",
-- >                "rules": [{"type": "is_selected_option", "option_index": 0}]}]}]}
--
-- Every @itemDetails@ key the IR has no field for is kept in 'qAttrs' as
-- @questionpunk:<key>@, so QuestionPunk-only types (@ai_interview@,
-- @max_diff@, ...) survive a trip through any format that keeps attributes.
module Panform.Format.QuestionPunkJson (readQPJson, writeQPJson, detailKeys) where

import Control.Monad (foldM, forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (find, groupBy, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Expr (pushNot)
import Panform.Json

-- | Every answer type the app stores (Prisma @AnswerType@).
answerTypes :: [Text]
answerTypes =
  authorable
    ++ [ "usability_test", "ai_question", "chat_reset", "distribution_builder", "button"
       , "text_highlight", "ai_image_generation", "concept_test", "concept_explore"
       , "concept_converge", "concept_final", "vignette_experiment", "list_experiment", "consent"
       , "design", "decision", "wiki_survey", "scheduling"
       ]

-- | The types the chat editor accepts on a new survey (@chatEditAnswerTypes@:
-- @generateAnswerTypes@ plus the pricing questions, the five-second test, Kano
-- and the unified AI interview); anything else makes the app reject the whole
-- payload.
authorable :: [Text]
authorable =
  [ "opinion_scale", "slider", "slider_matrix", "rating", "ranking", "matrix", "multiple_choice"
  , "dropdown", "long_text", "short_text", "email", "phone_number", "contact_form", "address"
  , "website", "date", "ai_interview", "ai_audio_interview", "chat_message", "page", "numeric"
  , "constant_sum", "audio_record", "video_record", "max_diff", "card_sorting", "conjoint_analysis"
  , "heatmap_click", "signature", "file_upload", "photo_response", "payment"
  , "linkedin_verification", "google_verification", "email_verification", "phone_verification"
  , "van_westendorp", "gabor_granger", "five_second_test", "kano", "ai_moderated_interview"
  ]

-- | Detail keys this writer derives from the IR that the app's chat-edit
-- parser does not accept (@CHAT_EDITABLE_ITEM_DETAIL_KEYS@).
notEditable :: [Text]
notEditable = ["allow_floating", "allow_negative", "min", "max", "min_length", "max_length", "include_time", "other_option_label", "none_option_label"]

-- | Every item-detail key the app declares, from
-- @ITEM_DETAIL_KEYS_BY_ANSWER_TYPE@, @TEXT_FIELD_ITEM_DETAIL_KEYS_BY_ANSWER_TYPE@
-- and @CHAT_EDITABLE_ITEM_DETAIL_KEYS@. QuestionPunk Markdown writes these
-- without the @questionpunk:@ prefix.
detailKeys :: [Text]
detailKeys =
  [ "action", "advanceType", "advancedSettings", "agentInstruction", "agree_label"
  , "aiAssistantEnabled", "aiDecidesConfig", "aiFailureBehavior", "aiFollowUpConfig", "aiModel"
  , "ai_suggest_options", "allowFeedback", "allowGreetingInterruption", "allowProposals"
  , "allowRating", "allow_cant_decide", "allow_decimals", "allow_floating", "allow_na"
  , "allow_negative", "allow_new_ideas", "allow_replay", "allow_rerecord", "allowed_types"
  , "amount_cents", "amount_source", "anchorLabelLayout", "anchorOptions", "anchor_option_ids"
  , "answer_format", "answer_options", "appendedEthics", "appendedObjectives", "appendedPrinciples"
  , "ask_importance", "aspectRatio", "assessmentGreeting", "assetIds", "assetUrls", "attributes"
  , "audioVideoMode", "autoAdvanceDelayMs", "avatarConfig", "avatarPanel", "avoidTopics"
  , "batteryQuestionsCount", "best_label", "bin_labels", "booking_url", "brief", "buttonAction"
  , "buttonAlignment", "buttonLabel", "buttonText", "buttonUrl", "button_label"
  , "calendly_answer_source_item_ids", "canvas_height", "capture_mode", "cards", "carryForwardFrom"
  , "categories", "cheap_label", "collect_signature", "columns", "competitorName", "completionCode"
  , "conceptTestId", "conceptType", "conceptsToRank", "control_statements", "conversationTurns"
  , "coverageLedger", "creationModes", "criteria", "criteria_source", "crowd_prediction"
  , "crowd_prediction_mode", "currency", "currency_prefix", "currency_suffix", "customContext"
  , "customProbes", "customSections", "custom_voice_lenses", "custom_voice_names", "decimals"
  , "default_country", "default_date", "default_value", "delayedSendSeconds", "deposit_percent"
  , "designProjectId", "discussionTopics", "displayMode", "display_mode", "divider"
  , "dropdown_options", "durationMinutes", "dysfunctional_questions", "email_source_item_id"
  , "enableModelFallback", "endOnStopRequest", "endSessionVisibility", "endSurvey"
  , "endSurveyImmediateRedirect", "endSurveyOutcome", "endSurveyRedirectUrl", "evaluationMethod"
  , "evaluationMode", "exclusiveOptionIndices", "exclusiveWith", "expensive_label"
  , "exposure_seconds", "fallbackModel", "fallbackTextMode", "feature_ids", "features"
  , "feedbackPlaceholder", "fields", "finalMrxConfig", "find_a_time", "find_a_time_zone"
  , "fixedQuestionCount", "followRespondentLanguage", "format", "functional_questions"
  , "generationConfig", "globalTimerSec", "greetingInstruction", "greetingVerbatim", "group"
  , "guidedTask", "guidedTaskText", "helpText", "highlightReelEnabled", "idea_approval"
  , "imageGenConfig", "imageGenPromptGuidance", "image_alt", "image_url", "include_none_option"
  , "include_time", "initialContextMessage", "inputMode", "instruction_text", "interviewAttributes"
  , "interviewGoal", "interviewQuestions", "isEndScreen", "items_per_set", "labelPosition"
  , "labels", "labelsPosition", "languageSwitchBehavior", "length", "linkOpenInSameTab"
  , "liveTranscriptEnabled", "liveTranscriptHistoryEnabled", "liveTranscriptShowUser", "max"
  , "maxGenerations", "maxRespondents", "max_amount_cents", "max_clicks", "max_date"
  , "max_file_size_mb", "max_files", "max_label", "max_length", "max_photos", "max_price"
  , "max_value", "max_value_label", "max_votes", "medium_value_label", "min", "min_amount_cents"
  , "min_clicks", "min_date", "min_label", "min_length", "min_price", "min_value"
  , "min_value_label", "min_votes", "mockupConfig", "modality", "mode", "modes"
  , "multiple_selection", "multiple_selection_max", "multiple_selection_min", "name_source_item_id"
  , "nms_enabled", "none_of_the_above_option", "none_option", "none_option_label", "nps"
  , "numWinnersToTest", "num_sets", "num_tasks", "numeric_labels", "openEndedQuestion"
  , "option_columns", "option_descriptions", "option_group_labels", "option_groups", "option_ids"
  , "option_image_alts", "option_images", "option_layout", "option_limits", "option_links"
  , "options", "options_source", "other_option", "other_option_label", "pair", "pairsToShow"
  , "passage", "payment_description", "payment_type", "pen_color", "personality", "phaseConfig"
  , "pinnedCardIndices", "pinnedOptionIndices", "pinnedRowIndices", "placeholder", "postAnalysis"
  , "postAnswerMessages", "practiceQuestion", "price_order", "priced", "prices", "profileField"
  , "profiles_per_task", "profilingQuestions", "prolificStudyId", "promptMode"
  , "promptSectionOverrides", "questionMode", "questionText", "quiz", "quizExplanation"
  , "randomize_cards", "randomize_features", "randomize_options", "randomize_rows"
  , "randomize_sets", "randomize_statement_order", "ranking_max", "ranking_min", "ratingScales"
  , "recordingMode", "referenceImages", "referenceQuestionsMode", "referenceSources"
  , "referenceUseAIFollowup", "referenceUsePreviousAnswer", "reference_questions"
  , "referencedConceptTestId", "requiredFieldIndices", "researchQuestions", "respondentControls"
  , "reveal_benchmark", "reveal_bonus", "reveal_dimensions", "reveal_importance", "reveal_segment"
  , "reveal_tags", "reveal_weights", "rightLabels", "routingMode", "rowColumns", "rows", "scale"
  , "scale_max_label", "scale_min_label", "scale_points", "screenShareDesktopRecordLocal"
  , "screenShareEnabled", "screenShareRequired", "semantic_differential", "sensitive_statement"
  , "sessionConfig", "shape", "share_results", "share_results_before", "share_results_claimed_only"
  , "share_results_details_from", "share_results_names_from", "share_results_tags_from"
  , "share_results_when", "showChatHistoryAfter", "showContinueButton", "showTransitionCard"
  , "show_letters", "show_recommendation", "show_steppers", "show_ticks", "show_value_input"
  , "skipIntro", "slider_step", "sort_mode", "stageConfig", "statement", "step"
  , "submit_button_label", "suggestAddresses", "suggestCountries", "tabulation_method"
  , "target_areas", "template", "thinkingMode", "thirdOption", "timeBasedCompletionConfig"
  , "timeLimitSeconds", "time_format", "timeoutMs", "timer", "timerMode", "too_cheap_label"
  , "too_expensive_label", "topics", "total_balls", "total_charge", "total_value", "triggerMode"
  , "unique_answer", "uploadedImages", "vadProfile", "value_prefix", "value_suffix", "variant"
  , "videoEnabled", "voices", "worst_label", "wrapUpWarningSeconds"
  ]

pfx :: Text
pfx = "questionpunk:"

tshow :: Int -> Text
tshow = T.pack . show

------------------------------------------------------------------------------
-- Reading

-- | One entry of @survey_items@, with item-level and nested details merged
-- the way the app merges them.
data Src = Src
  { srcN :: Int
  , srcType :: Text
  , srcText :: Text
  , srcRequired :: Bool
  , srcName :: Maybe Text
  , srcDetails :: M.Map Text Value
  }

readQPJson :: BL.ByteString -> Conv Form
readQPJson bs = do
  v <- eitherDecode bs `orFail` "QuestionPunk JSON"
  raws <- case key "survey_items" v of
    Just (Array a) -> pure (V.toList a)
    Just (String "unchanged") -> failWith "QuestionPunk JSON: survey_items is \"unchanged\", there is nothing to read"
    _ -> failWith "QuestionPunk JSON: survey_items should be an array"
  case v of
    Object o -> mapM_ (warn "questionpunk-json" . ("top-level field ignored: " <>)) [k | k <- map K.toText (KM.keys o), k `notElem` topKeys]
    _ -> pure ()
  srcs <- uniqueNames (zipWith srcOf [1 ..] raws)
  items <- mapM readItem srcs
  let byN = M.fromList (zip [1 ..] items)
  shown <- fmap catMaybes . mapM (displayEntry byN) $ keyArr "survey_display_logic" v
  jumps <- concat <$> mapM (branchEntry byN) (keyArr "survey_logic" v)
  let conds = M.fromListWith (flip (++)) [(n, [e]) | (n, e) <- shown ++ jumps]
      withRel n i = maybe i (setRel i . foldl1 (EBin And)) (M.lookup n conds)
  grouped <- sections (keyArr "survey_sections" v) (M.size byN) [(n, withRel n i) | (n, i) <- zip [1 ..] items]
  let settings = fromMaybe Null (key "survey_settings" v)
      title = keyText "survey_title" v `orElse` keyText "survey_title" settings
      screens = [(pfx <> k, x) | k <- ["welcome_screen_text", "thank_screen_text"], Just x <- [key k settings]]
      meta =
        emptyMeta
          { metaTitle = plain <$> title
          , metaDescription = plain <$> keyText "survey_description" settings
          , metaAttrs = M.fromList screens
          }
  pure (Form meta grouped)
  where
    orElse a b = maybe b Just a
    topKeys =
      [ "survey_items", "survey_title", "survey_settings", "survey_sections", "survey_logic"
      , "survey_display_logic", "assistant_message", "edit_action", "suggested_replies"
      ]

srcOf :: Int -> Value -> Src
srcOf n v =
  Src
    { srcN = n
    , srcType = keyTextOr "multiple_choice" "answer_type" v
    , srcText = keyTextOr "" "text" v
    , srcRequired = keyBool "required" v == Just True
    , srcName = keyText "internal_name" v >>= \t -> if T.null (T.strip t) then Nothing else Just t
    , srcDetails = M.union (objMap (key "itemDetails" v)) (M.filterWithKey (\k _ -> k `notElem` reserved) (objMap (Just v)))
    }
  where
    reserved = ["id", "text", "answer_type", "required", "internal_name", "itemDetails"]

objMap :: Maybe Value -> M.Map Text Value
objMap = \case
  Just (Object o) -> M.fromList [(K.toText k, x) | (k, x) <- KM.toList o]
  _ -> M.empty

-- | Question ids come from @internal_name@; a missing or repeated name falls
-- back to the position, @q3@, with a suffix when another question, earlier
-- or later, already has that name.
uniqueNames :: [Src] -> Conv [Src]
uniqueNames srcs = fmap (reverse . snd) (foldM step ([], []) srcs)
  where
    reserved = [n | Just n <- map srcName srcs]
    step (seen, acc) s = do
      let base = "q" <> tshow (srcN s)
          taken c = c `elem` reserved || c `elem` seen
          fallback = case filter (not . taken) (base : [base <> "_" <> tshow k | k <- [2 :: Int ..]]) of
            c : _ -> c
            [] -> base
      name <- case srcName s of
        Just n | n `elem` seen -> fallback <$ warn n ("repeated internal_name; item " <> tshow (srcN s) <> " renamed " <> fallback)
        Just n -> pure n
        Nothing -> pure fallback
      pure (name : seen, s {srcName = Just name} : acc)

readItem :: Src -> Conv Item
readItem s
  | srcType s == "chat_message" && M.null (srcDetails s) = pure (ItemNote name (plain (srcText s)) Nothing)
  | otherwise = do
      unless (srcType s `elem` answerTypes) $ warn name ("unknown answer type " <> srcType s <> " kept by name")
      let (qt, used, val, extra) = readType s
          rest = M.filterWithKey (\k _ -> k `notElem` used) (srcDetails s)
      pure . ItemQuestion $
        (question name (srcText s) qt)
          { qRequired = srcRequired s
          , qValidation = val
          , qAttrs = M.union extra (M.mapKeys (pfx <>) rest)
          }
  where
    name = fromMaybe "" (srcName s)

-- | The IR type, the detail keys it consumed, a validation and extra
-- attributes.
readType :: Src -> (QType, [Text], Maybe Validation, Attrs)
readType s = case srcType s of
  "multiple_choice" ->
    let other = flag "other_option"
        none = flag "none_option"
        ls = optionLabels "answer_options"
        oLabel = labelOr "Other" "other_option_label"
        nLabel = labelOr "None of the above" "none_option_label"
        (codes, oCode, nCode) = optionCodes ls (if other then Just oLabel else Nothing) (if none then Just nLabel else Nothing)
        cs =
          zipWith choice codes ls
            ++ [Choice c (plain oLabel) True False | Just c <- [oCode]]
            ++ [Choice c (plain nLabel) False True | Just c <- [nCode]]
     in plainT
          (QChoice (if flag "multiple_selection" then Multiple else Single) Radio cs)
          (["answer_options", "multiple_selection", "other_option", "none_option"] ++ ["other_option_label" | other] ++ ["none_option_label" | none])
  "dropdown" -> plainT (QChoice Single Dropdown (options "answer_options")) ["answer_options"]
  "ranking" -> plainT (QRanking (options "answer_options")) ["answer_options"]
  "matrix" ->
    plainT (QMatrix (if flag "multiple_selection" then Multiple else Single) (options "rows") (options "columns")) ["rows", "columns", "multiple_selection"]
  "opinion_scale" ->
    plainT (QScale (int 1 "min_value") (int 10 "max_value") (label "min_value_label") (label "max_value_label")) ["min_value", "max_value", "min_value_label", "max_value_label"]
  "rating" ->
    ( QScale 1 (int 5 "scale") (label "min_label") (label "max_label")
    , ["scale", "min_label", "max_label"]
    , Nothing
    , M.singleton (pfx <> "answer_type") (String "rating")
    )
  "short_text" -> textT ShortText
  "long_text" -> textT LongText
  "email" -> plainT (QText Email) []
  "website" -> plainT (QText Url) []
  "phone_number" -> plainT (QText Phone) []
  "numeric" ->
    ( QNumber (if flag "allow_floating" then Decimal else Integer)
    , ["allow_floating", "min", "max"]
    , bounds "min" "max"
    , M.empty
    )
  "date" -> plainT (QDate (if flag "include_time" then DateTime else Date)) ["include_time"]
  "file_upload" -> plainT QFile []
  other -> plainT (QOther other) []
  where
    d = srcDetails s
    get k = M.lookup k d
    flag k = case get k of
      Just (Bool b) -> b
      _ -> False
    int def k = maybe def round (get k >>= num)
    num = \case
      Number n -> Just (realToFrac n :: Double)
      _ -> Nothing
    str k = get k >>= asText
    label k = plain <$> (str k >>= \t -> if T.null t then Nothing else Just t)
    labelOr def k = maybe def (\t -> if T.null t then def else t) (str k)
    optionLabels k = case get k of
      Just (Array a) -> map (fromMaybe "" . asText) (V.toList a)
      _ -> []
    options k = let ls = optionLabels k; (codes, _, _) = optionCodes ls Nothing Nothing in zipWith choice codes ls
    plainT t used = (t, used, Nothing, M.empty)
    textT k = (QText k, ["min_length", "max_length"], bounds "min_length" "max_length", M.empty)
    bounds lo hi = case (get lo >>= num, get hi >>= num) of
      (Nothing, Nothing) -> Nothing
      (a, b) -> Just (Validation a b Nothing Nothing)

setRel :: Item -> Expr -> Item
setRel i e = case i of
  ItemQuestion q -> ItemQuestion q {qRelevant = Just (maybe e (`andE` e) (qRelevant q))}
  ItemNote n t r -> ItemNote n t (Just (maybe e (`andE` e) r))
  _ -> i
  where
    andE = EBin And

-- | A rule as an expression over its source question.
ruleExpr :: Item -> Value -> Either Text Expr
ruleExpr it r = case (keyTextOr "" "type" r, it) of
  ("is_selected_option", ItemQuestion q) -> do
    v <- optionValue q (fromMaybe Null (key "option_index" r `orElse` key "idx" r))
    pure $ case qType q of
      QChoice Single _ _ -> EBin Eq (ERef (qId q)) (ELit (LStr v))
      _ -> ESelected (qId q) v
  (t, ItemQuestion q) | Just op <- lookup t numRules -> case keyNum "value" r of
    Just n -> Right (EBin op (ERef (qId q)) (ELit (LNum n)))
    Nothing -> Left (t <> " rule without a numeric value")
  (t, ItemQuestion q) | Just op <- lookup t textRules -> Right (EBin op (ERef (qId q)) (ELit (LStr (keyTextOr "" "value" r))))
  (t, _) -> Left ("unsupported rule " <> t)
  where
    orElse a b = maybe b Just a

numRules, textRules :: [(Text, BinOp)]
numRules = [("is_number_greater", Gt), ("is_number_less", Lt), ("is_number_euqal_to", Eq), ("is_number_not_euqal_to", Neq)]
textRules = [("is", Eq), ("is_not", Neq)]

-- | The choice value an @option_index@ names: a position among the plain
-- options, or @"other"@ / @"none"@.
optionValue :: Question -> Value -> Either Text Text
optionValue q ix = do
  cs <- case qType q of
    QChoice _ _ cs -> Right cs
    QRanking cs -> Right cs
    _ -> Left ("option rule on " <> qId q <> ", which has no options")
  let regular = [c | c <- cs, not (choiceOther c || choiceExclusive c)]
      pick f = maybe (Left ("no " <> T.toLower (T.pack (show ix)) <> " option")) (Right . choiceValue) (find f cs)
  case ix of
    String "other" -> pick choiceOther
    String "none" -> pick choiceExclusive
    _ -> case keyNum "i" (Object (KM.singleton "i" ix)) of
      Just n | n >= 0, n == fromIntegral (round n :: Int), round n < length regular -> Right (choiceValue (regular !! round n))
      _ -> Left ("option index out of range on " <> qId q)

-- | Rules joined by a group's condition; groups by the container's.
joinWith :: Maybe Text -> [Expr] -> Maybe Expr
joinWith _ [] = Nothing
joinWith c es = Just (foldl1 (EBin (if c == Just "and" then And else Or)) es)

rulesOf :: M.Map Int Item -> Text -> Value -> Conv (Maybe Expr)
rulesOf byN at g = case M.lookup (maybe 0 round (keyNum "source_question" g)) byN of
  Nothing -> Nothing <$ warn at "condition on a question that does not exist dropped"
  Just src -> do
    es <- forM (keyArr "rules" g) $ \r -> either (\w -> Nothing <$ warn at ("rule dropped: " <> w)) (pure . Just) (ruleExpr src r)
    pure (joinWith (keyText "condition" g) (catMaybes es))

displayEntry :: M.Map Int Item -> Value -> Conv (Maybe (Int, Expr))
displayEntry byN e = case M.lookup target byN of
  Nothing -> Nothing <$ warn "questionpunk-json" "display logic for a question that does not exist dropped"
  Just it -> do
    gs <- mapM (rulesOf byN (itemId it)) (keyArr "groups" e)
    pure ((,) target <$> joinWith (keyText "container_condition" e) (catMaybes gs))
  where
    target = maybe 0 round (keyNum "target_question" e)

-- | Branching ("after Q2, if X go to Q5") as display conditions on the
-- questions it skips: Q3 and Q4 show only when no rule jumping past them
-- fired. Exact when the source question was reached.
branchEntry :: M.Map Int Item -> Value -> Conv [(Int, Expr)]
branchEntry byN e = case M.lookup s byN of
  Nothing -> [] <$ warn "questionpunk-json" "branching from a question that does not exist dropped"
  Just src -> do
    gs <- fmap catMaybes . forM (keyArr "groups" e) $ \g -> do
      c <- rulesOf byN (itemId src) (g `withSource` s)
      pure ((,) <$> c <*> pure (dest (key "destination_question" g)))
    let fallback = maybe (s + 1) (dest . Just) (key "fallback_question" e)
        conds = map fst gs
        firstMatch i c = if i == 0 then c else EBin And c (ENot (foldl1 (EBin Or) (take i conds)))
        noMatch = ENot (foldl1 (EBin Or) conds)
        reach = maximum (fallback : map snd gs)
        showK k =
          let fired = [firstMatch i c | (i, (c, dst)) <- zip [0 ..] gs, dst <= k]
              opts = fired ++ [noMatch | fallback <= k]
           in if length fired == length gs && fallback <= k
                then Nothing
                else Just (if null opts then ELit (LBool False) else foldl1 (EBin Or) opts)
        out = [(k, x) | not (null gs), k <- [s + 1 .. min (M.size byN) (reach - 1)], Just x <- [showK k]]
    when (any ((<= s) . snd) gs || fallback <= s) $
      warn (itemId src) "a branch back to the same or an earlier question is not carried; those respondents go on to the next question"
    unless (null out) $ warn (itemId src) "branching converted to display conditions on the questions it skips"
    pure out
  where
    s = maybe 0 round (keyNum "source_question" e)
    dest = \case
      Just (Number n) -> round n
      _ -> maxBound
    withSource g n = case g of
      Object o -> Object (KM.insert "source_question" (toJSON n) o)
      _ -> g

-- | Rebuild sections around the flat item list. A section whose items are
-- not contiguous is split so item order (and therefore logic) holds.
sections :: [Value] -> Int -> [(Int, Item)] -> Conv [Item]
sections secs total items = do
  let defs = [(k, sec) | (k, sec) <- zip [1 :: Int ..] secs, not (T.null (T.strip (keyTextOr "" "title" sec)))]
      members = M.fromListWith (\_ old -> old) [(n, k) | (k, sec) <- defs, n <- idxs sec, n >= 1, n <= total]
      runs = groupBy ((==) `on` (\(n, _) -> M.lookup n members)) items
  (_, out) <- foldM (build (M.fromList defs) members) (M.empty, []) runs
  pure (concat (reverse out))
  where
    idxs sec = [round n | Just n <- map (\x -> keyNum "i" (Object (KM.singleton "i" x))) (keyArr "item_indices" sec)]
    build defs members (seen, acc) run = case run of
      ((n, _) : _) | Just k <- M.lookup n members, Just sec <- M.lookup k defs -> do
        let times = M.findWithDefault 0 k seen :: Int
            gid = "section" <> tshow k <> (if times == 0 then "" else "_" <> tshow (times + 1))
            loop = key "repeat_from_data" sec
        when (times == 1) $ warn gid "section items are not contiguous; section split"
        let g =
              Group
                { groupId = gid
                , groupKind = maybe Section (const Repeat) loop
                , groupTitle = Just (plain (T.strip (keyTextOr "" "title" sec)))
                , groupRelevant = Nothing
                , groupItems = map snd run
                , groupAttrs = maybe M.empty (M.singleton (pfx <> "repeat_from_data")) loop
                }
        pure (M.insert k (times + 1) seen, [ItemGroup g] : acc)
      _ -> pure (seen, map snd run : acc)

------------------------------------------------------------------------------
-- Writing

-- | How a question lands in the app: its answer type and details, plus what
-- logic can say about it.
data Plan = Plan
  { pType :: Text
  , pDetails :: M.Map Text Value
  , pWarns :: [Text]
  , pOptions :: Maybe (Bool, [(Text, Value)])
  -- ^ Multi-select?, and choice value -> @option_index@.
  , pNumber :: Maybe Bool
  -- ^ Numeric rules apply; 'True' when answers are whole numbers.
  , pText :: Bool
  -- ^ Text equality rules apply.
  }

basic :: Text -> [(Text, Value)] -> Plan
basic t ds = Plan t (M.fromList ds) [] Nothing Nothing False

writeQPJson :: Form -> Conv BL.ByteString
writeQPJson (Form meta items) = do
  (entries, secs) <- flatten items
  let numbered = zip [1 :: Int ..] entries
      planned = [(n, it, case it of ItemQuestion q -> Just (plan q); _ -> Nothing) | (n, (it, _, _)) <- numbered]
      -- Conditions name a question; when two share a name the first keeps it
      -- (itemNames renames the later ones), so the first is the one meant.
      plans = M.fromListWith (\_ first -> first) [(itemId it, (n, p)) | (n, it, Just p) <- planned]
  names <- itemNames [(n, itemId it) | (n, (it, _, _)) <- numbered]
  -- Each question is written from its own plan.
  itemsJ <- forM (zip names planned) $ \(name, (_, it, p)) -> writeItem p name it
  logic <- fmap catMaybes . forM numbered $ \(n, (it, rel, _)) -> case rel of
    Nothing -> pure Nothing
    Just e -> fmap (routeTo (length numbered) n it) <$> displayLogic plans (itemId it) n e
  let members k = [n | (n, (_, _, Just k')) <- numbered, k' == k]
      secJ =
        [ obj ["title" .=? String t, "item_indices" .=? toJSON (members k), ("repeat_from_data" .=?) =<< loop]
        | (k, (t, loop)) <- zip [0 ..] secs
        , not (null (members k))
        ]
  mapM_ (\(_, (t, _)) -> warn t "empty section dropped; the app keeps no section without items") [x | x@(k, _) <- zip [0 ..] secs, null (members k)]
  settings <- metaSettings meta
  let langs = surveyLangs (Form meta items)
  unless (null langs) $ warn "survey" ("translations dropped (" <> T.intercalate ", " langs <> "); questionpunk-json carries one language")
  let jumps = [j | Left j <- logic]
      shows = [d | Right d <- logic]
  pure . encodePretty $
    obj
      [ ("survey_title" .=?) . String . i18nDefault =<< metaTitle meta
      , if M.null settings then Nothing else "survey_settings" .=? toJSON settings
      , "survey_items" .=? toJSON itemsJ
      , if null secJ then Nothing else "survey_sections" .=? toJSON secJ
      , if null jumps then Nothing else "survey_logic" .=? toJSON jumps
      , if null shows then Nothing else "survey_display_logic" .=? toJSON shows
      ]

metaSettings :: Meta -> Conv (M.Map Text Value)
metaSettings m = do
  maybe (pure ()) (\i -> warn i "survey id not carried") (metaId m)
  unless (null (metaLanguages m)) $ warn "survey" ("survey language (" <> T.intercalate ", " (metaLanguages m) <> ") not carried; the app sets it when the survey is created")
  screens <- fmap catMaybes . forM (M.toList (metaAttrs m)) $ \(k, v) -> case T.stripPrefix pfx k of
    Just s | s `elem` ["welcome_screen_text", "thank_screen_text"] -> pure (Just (s, v))
    _ -> Nothing <$ warn "survey" ("attribute " <> k <> " dropped")
  pure (M.fromList (screens ++ [("survey_description", String (i18nDefault d)) | Just d <- [metaDescription m]]))

-- | Every language any text is translated into.
surveyLangs :: Form -> [Lang]
surveyLangs (Form m is) = nub (sort (concatMap i18nLangs (catMaybes [metaTitle m, metaDescription m] ++ concatMap texts (allItems is))))
  where
    texts = \case
      ItemGroup g -> catMaybes [groupTitle g]
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q ->
        qLabel q : catMaybes [qHint q, qValidation q >>= valMessage] ++ case qType q of
          QChoice _ _ cs -> map choiceLabel cs
          QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
          QRanking cs -> map choiceLabel cs
          QScale _ _ l h -> catMaybes [l, h]
          _ -> []

-- | Items in order with their inherited condition and section. Top-level
-- groups become sections; the app has one level of them and no page breaks,
-- so a group that holds groups (a REDCap form with section headers) gives
-- way to the groups inside it.
flatten :: [Item] -> Conv ([(Item, Maybe Expr, Maybe Int)], [(Text, Maybe Value)])
flatten = foldM (top Nothing) ([], [])
  where
    top rel (acc, secs) = \case
      ItemGroup g | any isGroup (groupItems g) -> do
        warn (groupId g) "group of groups unwrapped; the inner groups become sections"
        foldM (top (conj rel (groupRelevant g))) (acc, secs) (groupItems g)
      ItemGroup g -> do
        let k = length secs
            loop = M.lookup (pfx <> "repeat_from_data") (groupAttrs g)
        case groupKind g of
          Page -> warn (groupId g) "page written as a section; page breaks are not carried"
          Repeat | isNothing loop -> warn (groupId g) "repeat group written as a plain section"
          _ -> pure ()
        mapM_ (\a -> warn (groupId g) ("attribute " <> a <> " dropped")) [a | a <- M.keys (groupAttrs g), a /= pfx <> "repeat_from_data"]
        kids <- inner (Just k) (conj rel (groupRelevant g)) (groupItems g)
        -- The app skips a section with a blank title.
        let fallback = if T.null (groupId g) then "Section " <> tshow (k + 1) else groupId g
            title = case T.strip . i18nDefault <$> groupTitle g of
              Just t | not (T.null t) -> t
              _ -> fallback
        pure (acc ++ kids, secs ++ [(title, loop)])
      i -> (\kids -> (acc ++ kids, secs)) <$> inner Nothing rel [i]
    isGroup = \case
      ItemGroup _ -> True
      _ -> False
    inner sec rel = fmap concat . mapM (one sec rel)
    one sec rel = \case
      ItemGroup g -> do
        warn (groupId g) "nested group flattened into its section"
        inner sec (conj rel (groupRelevant g)) (groupItems g)
      ItemQuestion q -> case qType q of
        QHidden -> [] <$ warn (qId q) "hidden field dropped; add it as a URL parameter in the app"
        QCalculate _ -> [] <$ warn (qId q) "calculation dropped"
        _ -> pure [(ItemQuestion q, conj rel (qRelevant q), sec)]
      ItemNote i t r -> pure [(ItemNote i t r, conj rel r, sec)]
      ItemRaw (Format "questionpunk-json") v -> pure [(ItemRaw (Format "questionpunk-json") v, rel, sec)]
      ItemRaw (Format f) _ -> [] <$ warn "questionpunk-json" ("raw " <> f <> " content dropped")
    conj a b = case (a, b) of
      (Just x, Just y) -> Just (EBin And x y)
      _ -> maybe b Just a

-- | Unique, non-blank @internal_name@s. The reader renames a repeated name
-- to its position (@q3@), so the writer does the same up front and says so;
-- a name already taken by then gets a suffix.
itemNames :: [(Int, Text)] -> Conv [Text]
itemNames = fmap (reverse . snd) . foldM step (M.empty :: M.Map Text (), [])
  where
    step (seen, acc) (n, i) = do
      let blank = T.null (T.strip i)
          candidates = ["q" <> tshow n] ++ ["q" <> tshow n <> "_" <> tshow k | k <- [2 :: Int ..]]
          fresh = headOr "" [c | c <- candidates, not (M.member c seen)]
      name <-
        if not blank && not (M.member i seen)
          then pure i
          else do
            unless blank $ warn i ("repeated name; item " <> tshow n <> " written as " <> fresh)
            pure fresh
      pure (M.insert name () seen, name : acc)
    headOr d = \case
      (x : _) -> x
      [] -> d

writeItem :: Maybe Plan -> Text -> Item -> Conv Value
writeItem mp name = \case
  ItemNote _ t _ -> pure (obj [nameField, "text" .=? String (i18nDefault t), "answer_type" .=? "chat_message", "required" .=? Bool False])
  ItemRaw _ v -> pure v
  ItemGroup g -> failWith ("group " <> groupId g <> " left after flattening")
  ItemQuestion q -> do
    let p = fromMaybe (plan q) mp
    mapM_ (warn (qId q)) (pWarns p)
    forM_' (qHint q) $ \_ -> warn (qId q) "hint dropped; the app has no question description"
    forM_' (qDefault q) $ \_ -> warn (qId q) "default value dropped"
    forM_' (qValidation q >>= valExpr) $ \_ -> warn (qId q) "validation rule dropped"
    forM_' (qValidation q >>= valMessage) $ \_ -> warn (qId q) "validation message dropped"
    attrs <- fmap catMaybes . forM (M.toList (qAttrs q)) $ \(k, v) -> case T.stripPrefix pfx k of
      Just "answer_type" -> pure Nothing
      Just s -> pure (Just (s, v))
      Nothing | k `elem` detailKeys -> pure (Just (k, v))
      Nothing -> Nothing <$ warn (qId q) ("attribute " <> k <> " dropped")
    let details = M.union (pDetails p) (M.fromList attrs)
        -- Written for a lossless round trip, but not on the chat editor's
        -- CHAT_EDITABLE_ITEM_DETAIL_KEYS list, so the app parser drops them.
        ignored = [k | (k, v) <- M.toList (pDetails p), k `elem` notEditable, v /= Bool False]
    unless (null ignored) $ warn (qId q) ("the app's chat editor ignores " <> T.intercalate ", " ignored <> "; its defaults apply")
    pure $
      obj
        [ nameField
        , "text" .=? String (i18nDefault (qLabel q))
        , "answer_type" .=? String (pType p)
        , "required" .=? Bool (qRequired q)
        , if M.null details then Nothing else "itemDetails" .=? toJSON details
        ]
  where
    nameField = "internal_name" .=? String name
    forM_' x f = maybe (pure ()) f x

plan :: Question -> Plan
plan q = case qType q of
  QText k ->
    let t = case k of
          ShortText -> "short_text"
          LongText -> "long_text"
          Email -> "email"
          Url -> "website"
          Phone -> "phone_number"
        lengths = k `elem` [ShortText, LongText]
        p = basic t (if lengths then bounds "min_length" "max_length" else [])
     in p {pText = lengths, pWarns = ["length limits dropped" | not lengths, hasBounds]}
  QNumber k ->
    (basic "numeric" (("allow_floating", Bool (k == Decimal)) : bounds "min" "max" ++ [("allow_negative", Bool True) | maybe False (< 0) (vmin)]))
      { pNumber = Just (k == Integer)
      , pWarns = ["no bounds given; the app defaults to 0-9999" | not hasBounds]
      }
  QDate k ->
    (basic "date" [("include_time", Bool True) | k /= Date])
      {pWarns = ["time-only question written as date and time" | k == Time]}
  QFile -> basic "file_upload" []
  QGeo -> (basic "short_text" []) {pWarns = ["location written as a text box"]}
  QBoolean ->
    (choicePlan "multiple_choice" Single [choice "true" "Yes", choice "false" "No"])
      { pWarns = ["yes/no written as a two-option choice"]
      , pOptions = Just (False, [(v, toJSON i) | (vs, i) <- [(["true", "yes", "1"], 0 :: Int), (["false", "no", "0"], 1)], v <- vs])
      }
  QChoice sel style cs -> case (sel, style) of
    (Single, Dropdown) -> choicePlan "dropdown" Single cs
    (Multiple, Dropdown) -> warnP "multi-select dropdown written as checkboxes" (choicePlan "multiple_choice" Multiple cs)
    (_, Buttons) -> warnP "button style written as a plain list" (choicePlan "multiple_choice" sel cs)
    _ -> choicePlan "multiple_choice" sel cs
  QRanking cs ->
    (basic "ranking" [("answer_options", labels cs)])
      { pOptions = Just (True, indexed cs)
      , pWarns = codeWarn cs
      }
  QMatrix sel rows cols ->
    (basic "matrix" [("rows", labels rows), ("columns", labels cols), ("multiple_selection", Bool (sel == Multiple))])
      {pWarns = codeWarn rows ++ codeWarn cols}
  QScale lo hi l h
    | M.lookup (pfx <> "answer_type") (qAttrs q) == Just (String "rating"), lo == 1, hi >= 5, hi <= 11 ->
        (basic "rating" (("scale", toJSON hi) : scaleLabels "min_label" "max_label"))
          {pNumber = Just True}
    | lo `elem` [0, 1], hi >= 5, hi <= 10 ->
        (basic "opinion_scale" ([("min_value", toJSON lo), ("max_value", toJSON hi)] ++ scaleLabels "min_value_label" "max_value_label"))
          {pNumber = Just True}
    | hi >= lo, toInteger hi - toInteger lo < 20 ->
        let point v = choice (tshow v) (tshow v <> maybe "" (\t -> " (" <> i18nDefault t <> ")") (if v == lo then l else if v == hi then h else Nothing))
         in warnP ("scale " <> tshow lo <> "-" <> tshow hi <> " written as a choice list; the app's scales run 0/1 to 5-10") (choicePlan "multiple_choice" Single (map point [lo .. hi]))
    | otherwise ->
        (basic "numeric" [("allow_floating", Bool False), ("min", toJSON lo), ("max", toJSON hi), ("allow_negative", Bool (lo < 0))])
          { pNumber = Just True
          , pWarns = ["scale " <> tshow lo <> "-" <> tshow hi <> " written as a number box"]
          }
    where
      scaleLabels a b = [(a, String (i18nDefault t)) | Just t <- [l]] ++ [(b, String (i18nDefault t)) | Just t <- [h]]
  QOther t
    | t `elem` authorable -> basic t []
    | t `elem` answerTypes -> (basic t []) {pWarns = [t <> " is not in the chat editor's allowlist; the app rejects it on a new survey"]}
    | otherwise -> (basic "short_text" []) {pWarns = ["unsupported type " <> t <> " written as a text box"]}
  QHidden -> basic "short_text" []
  QCalculate _ -> basic "short_text" []
  where
    val = qValidation q
    vmin = val >>= valMin
    vmax = val >>= valMax
    hasBounds = not (isNothing vmin && isNothing vmax)
    bounds a b = [(a, numV x) | Just x <- [vmin]] ++ [(b, numV x) | Just x <- [vmax]]
    warnP w p = p {pWarns = w : pWarns p}

labels :: [Choice] -> Value
labels = toJSON . map (i18nDefault . choiceLabel)

indexed :: [Choice] -> [(Text, Value)]
indexed cs = [(choiceValue c, toJSON i) | (i, c) <- zip [0 :: Int ..] cs]

-- | The app keeps options by position, with no codes. Reading gives each
-- option its label as its code when the labels are distinct and non-empty,
-- else its position (and @other@ / @none@ for the built-in options).
optionCodes :: [Text] -> Maybe Text -> Maybe Text -> ([Text], Maybe Text, Maybe Text)
optionCodes ls o n
  | not (any T.null every), length (nub every) == length every = (ls, o, n)
  | otherwise = (map tshow [1 .. length ls], "other" <$ o, "none" <$ n)
  where
    every = ls ++ catMaybes [o, n]

codeWarning :: Text
codeWarning = "choice codes dropped; the app keeps options by position, so codes come back as labels"

codeWarn :: [Choice] -> [Text]
codeWarn cs =
  let ls = map (i18nDefault . choiceLabel) cs
      (codes, _, _) = optionCodes ls Nothing Nothing
   in [codeWarning | map choiceValue cs /= codes]

-- | A choice list as the app stores it: plain options, then the built-in
-- "other" and "none of the above" options.
choicePlan :: Text -> Selection -> [Choice] -> Plan
choicePlan t sel cs0 =
  (basic t details)
    { pOptions = Just (sel == Multiple, indexed regular ++ [(choiceValue c, "other") | Just c <- [other]] ++ [(choiceValue c, "none") | Just c <- [none]])
    , pWarns = ws ++ codes ++ allAbove
    }
  where
    mc = t == "multiple_choice"
    explicitOther = if mc then find choiceOther cs0 else Nothing
    none = if mc then find (\c -> choiceExclusive c && Just c /= explicitOther) cs0 else Nothing
    rest0 = [c | c <- cs0, Just c /= explicitOther, Just c /= none]
    -- The app turns an option labelled "Other" into its built-in one.
    literal = if mc then find (otherLike . choiceLabel) rest0 else Nothing
    other = maybe literal Just explicitOther
    regular = [c | c <- rest0, Just c /= literal]
    ws =
      [ "option \"" <> i18nDefault (choiceLabel c) <> "\" becomes the app's other option" | isNothing explicitOther, Just c <- [literal]]
        ++ ["option \"" <> i18nDefault (choiceLabel c) <> "\" dropped; the app folds it into the other option" | Just _ <- [explicitOther], Just c <- [literal]]
        ++ ["other/none flags dropped; a dropdown has no built-in options" | not mc, any (\c -> choiceOther c || choiceExclusive c) cs0]
        ++ ["extra other/none option written as a plain option" | mc, any (\c -> choiceOther c || choiceExclusive c) regular]
    -- The labels the reader will see: an empty built-in label reads back
    -- as the app's default.
    readLabel def c = let l = i18nDefault (choiceLabel c) in if T.null l then def else l
    (expected, oCode, nCode) =
      optionCodes (map (i18nDefault . choiceLabel) regular) (readLabel "Other" <$> other) (readLabel "None of the above" <$> none)
    codes =
      [ codeWarning
      | map choiceValue regular /= expected
          || (choiceValue <$> other) /= oCode
          || (choiceValue <$> none) /= nCode
      ]
    allAbove = ["\"all of the above\" makes the app switch this question to multi-select" | mc, sel == Single, any (allLike . choiceLabel) regular]
    details =
      [("answer_options", labels regular)]
        ++ if mc
          then
            [ ("multiple_selection", Bool (sel == Multiple))
            , ("other_option", Bool (not (isNothing other)))
            , ("none_option", Bool (not (isNothing none)))
            ]
              ++ [("other_option_label", String (i18nDefault (choiceLabel c))) | Just c <- [other], i18nDefault (choiceLabel c) /= "Other"]
              ++ [("none_option_label", String (i18nDefault (choiceLabel c))) | Just c <- [none], i18nDefault (choiceLabel c) /= "None of the above"]
          else []
    norm = T.strip . T.unwords . T.words . T.filter (`notElem` (".!?" :: String)) . T.toLower . i18nDefault
    otherLike l = norm l `elem` ["other", "other (please specify)", "other (specify)", "others"]
    allLike l = norm l `elem` ["all of the above", "all the above", "all above"]

numV :: Double -> Value
numV x
  | x == fromIntegral (round x :: Integer) = toJSON (round x :: Integer)
  | otherwise = toJSON x

------------------------------------------------------------------------------
-- Display logic

-- | A condition as the app's container: groups of rules, each group about
-- one earlier question.
data Container = Container BinOp [(Int, BinOp, [Value])]

containerJSON :: Int -> Container -> Value
containerJSON target (Container op gs) =
  obj
    [ "target_question" .=? toJSON target
    , "container_condition" .=? opJSON op
    , "groups" .=? toJSON [obj ["source_question" .=? toJSON s, "condition" .=? opJSON o, "rules" .=? toJSON rs] | (s, o, rs) <- gs]
    ]

opJSON :: BinOp -> Value
opJSON o = String (if o == And then "and" else "or")

-- | A condition on a message that ends the survey goes out as branching
-- from the question just before it ("if screened out, go to the message,
-- else skip it"): the app requires a mid-survey end message to be reached
-- that way and otherwise invents the branch itself. Everything else is
-- display logic.
routeTo :: Int -> Int -> Item -> Container -> Either Value Value
routeTo total n it c@(Container op gs)
  | endsSurvey, n < total, op == Or || length gs == 1, all (\(s, _, _) -> s == n - 1) gs =
      Left $
        obj
          [ "source_question" .=? toJSON (n - 1)
          , "groups" .=? toJSON [obj ["condition" .=? opJSON o, "destination_question" .=? toJSON n, "rules" .=? toJSON rs] | (_, o, rs) <- gs]
          , "fallback_question" .=? toJSON (n + 1)
          ]
  | otherwise = Right (containerJSON n c)
  where
    endsSurvey = case it of
      ItemQuestion q -> qType q == QOther "chat_message" && M.lookup (pfx <> "endSurvey") (qAttrs q) == Just (Bool True)
      _ -> False

displayLogic :: M.Map Text (Int, Plan) -> Text -> Int -> Expr -> Conv (Maybe Container)
displayLogic env at target e0 = case container of
  Right c -> pure (Just c)
  Left why -> case parts e of
    (And, ps@(_ : _ : _)) -> do
      kept <- fmap catMaybes . forM ps $ \p -> either (\w -> Nothing <$ warn at ("part of the condition dropped, " <> w)) (const (pure (Just p))) (group p)
      -- What is left is written the way it will read back.
      pure $ case kept of
        [] -> Nothing
        _ -> either (const Nothing) Just (containerOf (foldl1 (EBin And) kept))
    _ -> Nothing <$ warn at ("logic dropped, " <> why)
  where
    e = norm e0
    container = containerOf e
    containerOf x0 = case parts x0 of
      (op, ps@(_ : _ : _)) -> either (const (Container op <$> mapM (cluster op) (runs ps))) (\g -> Right (Container Or [g])) (group x0)
      _ -> (\g -> Container Or [g]) <$> group x0
    -- Neighbouring parts on one question share a group, as they read back.
    runs = groupBy ((==) `on` (\p -> either (const Nothing) (\(src, _, _) -> Just src) (group p)))
    cluster op = \case
      [p] -> group p
      ps -> group (foldl1 (EBin op) ps)
    group x = do
      let (op, ps) = parts x
      ls <- mapM leaf ps
      src <- case nub [s | (s, _, _) <- ls] of
        [s] -> Right s
        _ -> Left "a group of rules must test one question"
      when (src >= target) $ Left "the condition reads a later question"
      let needOr = any (\(_, rs, o) -> o && length rs > 1) ls
      when (needOr && op == And && length ls > 1) $ Left "a negated choice cannot be combined with and"
      pure (src, if needOr then Or else op, concat [rs | (_, rs, _) <- ls])
    leaf = \case
      ESelected q v -> choiceRule q Eq (LStr v)
      EBin op (ERef q) (ELit l) -> case lookupPlan q of
        Right (_, p) | Just _ <- pOptions p -> choiceRule q op l
        Right (n, p) | Just whole <- pNumber p -> numRule n whole op l
        Right (n, p) | pText p, op `elem` [Eq, Neq] -> Right (n, [rule (if op == Eq then "is" else "is_not") (String (litText l))], False)
        Right (_, p) -> Left ("the app has no " <> T.pack (show op) <> " rule for " <> pType p <> " (" <> q <> ")")
        Left w -> Left w
      EAnswered q -> Left ("\"" <> q <> " is answered\" has no rule in the app")
      ENot (EAnswered q) -> Left ("\"" <> q <> " is empty\" has no rule in the app")
      ENot _ -> Left "negated choice on a multi-select question"
      x -> Left ("unsupported condition " <> T.take 60 (T.pack (show x)))
    lookupPlan q = maybe (Left ("unknown question " <> q)) Right (M.lookup q env)
    choiceRule q op l = do
      (n, p) <- lookupPlan q
      (multi, opts) <- maybe (Left (q <> " has no options")) Right (pOptions p)
      -- Anything but equality becomes "one of these options", which on a
      -- single-select question means the same thing once it is answered.
      ixs <- case op of
        Eq -> maybe (Left ("no option " <> litText l <> " in " <> q)) (Right . pure) (lookup (litText l) opts)
        _ | multi -> Left ("the app has no " <> T.pack (show op) <> " rule for a multi-select question (" <> q <> ")")
        Neq -> Right [i | i <- nub (map snd opts), Just i /= lookup (litText l) opts]
        _ -> do
          x <- maybe (Left ("non-numeric value " <> litText l)) Right (litNum l)
          f <- maybe (Left ("the app has no " <> T.pack (show op) <> " rule")) Right (lookup op [(Lt, (< x)), (Le, (<= x)), (Gt, (> x)), (Ge, (>= x))])
          Right (nub [i | (code, i) <- opts, Just c <- [litNum (LStr code)], f c])
      when (null ixs) $ Left ("no option of " <> q <> " satisfies the condition")
      Right (n, [rule "is_selected_option" i | i <- ixs], length ixs > 1)
    numRule n whole op l = do
      x <- maybe (Left ("non-numeric value " <> litText l)) Right (litNum l)
      let intX = x == fromIntegral (round x :: Integer)
      (t, v) <- case op of
        Eq -> Right ("is_number_euqal_to", x)
        Neq -> Right ("is_number_not_euqal_to", x)
        Lt -> Right ("is_number_less", x)
        Gt -> Right ("is_number_greater", x)
        Le | whole && intX -> Right ("is_number_less", x + 1)
        Ge | whole && intX -> Right ("is_number_greater", x - 1)
        _ -> Left ("the app has no " <> T.pack (show op) <> " rule for decimals")
      Right (n, [rule t (numV v)], False)
    rule t v = obj ["type" .=? String t, (if t == "is_selected_option" then "option_index" else "value") .=? v]

-- | Negations pushed onto comparisons where they can go.
norm :: Expr -> Expr
norm = \case
  ENot x -> let x' = norm x in maybe (ENot x') norm (pushNot x' >>= \y -> if isNot y then Nothing else Just y)
  EBin o a b -> EBin o (norm a) (norm b)
  x -> x
  where
    isNot = \case
      ENot _ -> True
      _ -> False

-- | An and/or chain as its operator and operands.
parts :: Expr -> (BinOp, [Expr])
parts e = case e of
  EBin op _ _ | op `elem` [And, Or] -> (op, go op e)
  _ -> (Or, [e])
  where
    go op x = case x of
      EBin o a b | o == op -> go op a ++ go op b
      _ -> [x]

litText :: Lit -> Text
litText = \case
  LStr s -> s
  LNum n -> if n == fromIntegral (round n :: Integer) then T.pack (show (round n :: Integer)) else T.pack (show n)
  LBool b -> if b then "true" else "false"

litNum :: Lit -> Maybe Double
litNum = \case
  LNum n -> Just n
  LStr s | not (T.null s), T.all (\c -> isDigit c || c `elem` ("-." :: String)) s, [(n, "")] <- reads (T.unpack s) -> Just n
  _ -> Nothing
