-- | SurveyMonkey API v3 survey JSON: the @GET /surveys/{id}/details@
-- response on the way in, the @POST /surveys@ body on the way out. Pages
-- hold questions typed by @family@/@subtype@; choices carry text but no
-- codes, and neither skip logic nor question names can be set through the
-- API, so those are lost on writing.
module Panform.Format.SurveyMonkey (readSurveyMonkey, writeSurveyMonkey) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Json

-- | Question keys carried as @surveymonkey:<key>@ attributes.
carried :: [Text]
carried = ["sorting", "forced_ranking", "quiz_options"]

defaultNumber :: Text
defaultNumber = "The answer must be a number."

defaultRequired :: Text
defaultRequired = "This question requires an answer."

------------------------------------------------------------------------------
-- Reading

readSurveyMonkey :: BL.ByteString -> Conv Form
readSurveyMonkey bs = do
  v <- eitherDecode bs `orFail` "SurveyMonkey JSON"
  let pages = keyArr "pages" v
      numbered = number 1 (map (keyArr "questions") pages)
  items <- forM (zip3 [1 :: Int ..] pages numbered) $ \(n, p, qs) -> do
    let pid = "page" <> maybe (T.pack (show n)) id (keyText "id" p)
        desc = [ItemNote (pid <> "_description") (plain d) Nothing | Just d <- [keyText "description" p], not (T.null d)]
    kids <- catMaybes <$> mapM (uncurry readQuestion) qs
    pure . ItemGroup $
      Group
        { groupId = pid
        , groupKind = Page
        , groupTitle = plain <$> nonEmpty (keyText "title" p)
        , groupRelevant = Nothing
        , groupItems = desc ++ kids
        , groupAttrs = M.empty
        }
  let meta =
        emptyMeta
          { metaId = keyText "id" v
          , metaTitle = plain <$> nonEmpty (keyText "title" v)
          , metaLanguages = maybe [] pure (keyText "language" v)
          , metaAttrs = M.fromList [("surveymonkey:nickname", x) | Just x <- [key "nickname" v]]
          }
  pure (Form meta items)
  where
    number _ [] = []
    number k (qs : rest) = zip [k ..] qs : number (k + length qs) rest

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null (T.strip t)) -> Just t
  _ -> Nothing

readQuestion :: Int -> Value -> Conv (Maybe Item)
readQuestion n q
  | keyBool "visible" q == Just False = Nothing <$ warn qid "deleted question skipped"
  | otherwise = do
      when (length headings > 1) $ warn qid "random-assignment heading variants dropped"
      case (family, subtype) of
        ("presentation", _) -> do
          when (subtype == "image" || any (isJust . key "image") headings) $ warn qid "image dropped"
          pure (Just (ItemNote qid (plain heading) Nothing))
        ("open_ended", st) | st `elem` ["multi", "numerical"] -> Just <$> textRows st
        _ -> do
          (qt, val) <- readType
          pure . Just . ItemQuestion $
            Question
              { qId = qid
              , qLabel = plain heading
              , qHint = Nothing
              , qType = qt
              , qRequired = required
              , qRelevant = Nothing
              , qValidation = val
              , qDefault = Nothing
              , qAttrs = attrs
              }
  where
    qid = "q" <> fromMaybe (T.pack (show n)) (keyText "id" q)
    headings = keyArr "headings" q
    heading = case headings of
      (h : _) -> keyTextOr "" "heading" h
      [] -> ""
    family = keyTextOr "" "family" q
    subtype = keyTextOr "" "subtype" q
    display = key "display_options" q
    displayType = display >>= keyText "display_type"
    answers = fromMaybe Null (key "answers" q)
    required = isJust (key "required" q)
    requiredText = [("surveymonkey:required_text", String t) | Just r <- [key "required" q], Just t <- [keyText "text" r], t /= defaultRequired]
    -- A demographic question is kept by name; its subtype and its answer
    -- rows are what the writer needs to write it back.
    demographic = [("surveymonkey:subtype", String subtype) | family == "demographic", not (T.null subtype)] ++ [("surveymonkey:answers", a) | family == "demographic", Just a <- [key "answers" q]]
    attrs = M.fromList ([("surveymonkey:" <> k, x) | k <- carried, Just x <- [key k q]] ++ requiredText ++ demographic)
    validation = key "validation" q
    vType = validation >>= keyText "type"
    vMessage = plain <$> nonEmpty (validation >>= keyText "text")
    vMin = validation >>= keyNum "min"
    vMax = validation >>= keyNum "max"
    rows = keyArr "rows" answers
    choices = [c | c <- keyArr "choices" answers, keyBool "visible" c /= Just False]
    readChoices = do
      when (any ((== Just False) . keyBool "visible") (keyArr "choices" answers)) $ warn qid "hidden choices skipped"
      when (any (isJust . key "image") choices) $ warn qid "choice images dropped"
      other <- readOther
      pure (zipWith (readChoice False) [1 ..] choices ++ other)
    readOther = case key "other" answers of
      Just (Array a) | (o : _) <- foldr (:) [] a -> otherChoice o
      Just o@(Object _) -> otherChoice o
      _ -> pure []
    otherChoice o
      | keyBool "visible" o == Just False = pure []
      | keyBool "is_answer_choice" o == Just False = [] <$ warn qid "comment box dropped"
      | otherwise = pure [Choice "other" (plain (keyTextOr "Other" "text" o)) True False]
    readType = case family of
      "single_choice" -> do
        cs <- readChoices
        pure (QChoice Single (style subtype) cs, Nothing)
      "multiple_choice" -> do
        cs <- readChoices
        pure (QChoice Multiple (style subtype) cs, Nothing)
      "open_ended"
        | displayType == Just "file_upload" -> pure (QFile, Nothing)
        | displayType == Just "slider" ->
            pure
              ( QScale
                  (maybe 0 round vMin)
                  (maybe 100 round vMax)
                  (plain <$> nonEmpty (display >>= keyText "left_label"))
                  (plain <$> nonEmpty (display >>= keyText "right_label"))
              , Nothing
              )
        | subtype == "essay" -> pure (QText LongText, lengthValidation)
        | otherwise -> case vType of
            Just "integer" -> pure (QNumber Integer, rangeValidation)
            Just "decimal" -> pure (QNumber Decimal, rangeValidation)
            Just "email" -> pure (QText Email, Nothing)
            Just t | t `elem` ["date_us", "date_intl"] -> do
              when (isJust vMin || isJust vMax) $ warn qid "date range dropped"
              pure (QDate Date, Nothing)
            Just "regex" -> (QText ShortText, Nothing) <$ warn qid "regex validation dropped"
            _ -> pure (QText ShortText, lengthValidation)
      "matrix" -> readMatrix
      "datetime" -> do
        when (length rows > 1) $ warn qid "extra date/time boxes dropped"
        pure (QDate (case subtype of "date_only" -> Date; "time_only" -> Time; _ -> DateTime), Nothing)
      "demographic" -> pure (QOther "demographic", Nothing)
      f -> (QOther f, Nothing) <$ warn qid ("unsupported family " <> f <> " kept by name")
    lengthValidation = if vType == Just "text_length" then Just (Validation vMin vMax Nothing vMessage) else Nothing
    rangeValidation
      | isJust vMin || isJust vMax || (isJust vMessage && vMessage /= Just (plain defaultNumber)) = Just (Validation vMin vMax Nothing vMessage)
      | otherwise = Nothing
    style = \case
      "menu" -> Dropdown
      "horiz" -> Buttons
      _ -> Radio
    readMatrix = case subtype of
      "ranking" -> pure (QRanking (zipWith (readChoice False) [1 ..] rows), Nothing)
      "menu" -> case keyArr "cols" answers of
        [c] -> do
          warn qid "dropdown matrix written as a radio matrix"
          pure (QMatrix Single (zipWith (readChoice False) [1 ..] rows) (zipWith (readChoice False) [1 ..] (keyArr "choices" c)), Nothing)
        _ -> (QOther "matrix_menu", Nothing) <$ warn qid "multi-column dropdown matrix kept by name"
      st -> do
        let rated = st == "rating"
            cols = zipWith (readChoice rated) [1 ..] choices
        when (isJust (key "other" answers)) $ warn qid "matrix comment box dropped"
        pure $ case scaleOf rated of
          Just s -> (s, Nothing)
          Nothing -> (QMatrix (if st == "multi" then Multiple else Single) (zipWith (readChoice False) [1 ..] rows) cols, Nothing)
    -- A one-row rating matrix whose weights run lo..hi is a rating scale.
    scaleOf rated = do
      [r] <- Just rows
      unless' (rated && T.null (T.strip (keyTextOr "" "text" r)))
      ws <- mapM (fmap round . keyNum "weight") choices
      (lo, hi) <- case ws of
        (a : _) -> Just (a, last ws)
        [] -> Nothing
      unless' (ws == [lo .. hi] && hi > lo)
      let texts = map (keyTextOr "" "text") choices
          end w t = if T.null t || t == T.pack (show w) then Nothing else Just (plain t)
          middle = drop 1 (init (zip ws texts))
      unless' (all (\(w, t) -> T.null t || t == T.pack (show w)) middle || displayType == Just "emoji")
      Just (QScale lo hi (end lo (headOr "" texts)) (end hi (lastOr "" texts)))
    unless' b = if b then Just () else Nothing
    headOr d = \case
      (x : _) -> x
      [] -> d
    lastOr d xs = if null xs then d else last xs
    -- @open_ended@ multi/numerical: one text box per row.
    textRows st = do
      when (isJust (validation >>= key "sum")) $ warn qid "sum validation dropped"
      let kind = if st == "numerical" then QNumber Decimal else QText ShortText
          subs =
            [ ItemQuestion (question (qid <> "_" <> choiceValue c) (i18nDefault (choiceLabel c)) kind) {qRequired = required}
            | c <- zipWith (readChoice False) [1 ..] rows
            ]
      pure . ItemGroup $
        Group
          { groupId = qid
          , groupKind = Section
          , groupTitle = Just (plain heading)
          , groupRelevant = Nothing
          , groupItems = subs
          , groupAttrs = M.fromList [("surveymonkey:subtype", String st)]
          }

-- | A choice's code: its weight on a rating scale, else its id, else its
-- position.
readChoice :: Bool -> Int -> Value -> Choice
readChoice useWeight n c =
  let val = fromMaybe (T.pack (show n)) ((if useWeight then keyText "weight" c else Nothing) `orElse` keyText "id" c)
   in Choice val (plain (keyTextOr "" "text" c)) False (keyBool "is_na" c == Just True)
  where
    orElse a b = maybe b Just a

------------------------------------------------------------------------------
-- Writing

writeSurveyMonkey :: Form -> Conv BL.ByteString
writeSurveyMonkey s@(Form meta items) = do
  unless (null (questions s)) $ warn "surveymonkey" "question names and choice codes not carried, SurveyMonkey assigns its own ids"
  when (hasTranslations s) $ warn "surveymonkey" "translations dropped, SurveyMonkey keeps each language as a separate survey"
  when (isJust (metaDescription meta)) $ warn "surveymonkey" "survey description dropped"
  pages <- forM (paginate items) $ \(g, kids) -> do
    forM_' g $ \x -> when (isJust (groupRelevant x)) $ warn (groupId x) "skip logic dropped, the SurveyMonkey API cannot set it"
    qs <- concat <$> mapM writeItem kids
    pure (obj [("title" .=?) . String =<< nonEmpty (i18nDefault <$> (g >>= groupTitle)), "questions" .=? toJSON qs])
  pure . encodePretty . obj $
    [ ("title" .=?) . String . i18nDefault =<< metaTitle meta
    , case metaLanguages meta of
        (l : _) -> "language" .=? String l
        [] -> Nothing
    , ("nickname" .=?) =<< M.lookup "surveymonkey:nickname" (metaAttrs meta)
    , "pages" .=? toJSON pages
    ]
  where
    forM_' m f = maybe (pure ()) f m

hasTranslations :: Form -> Bool
hasTranslations (Form meta items) =
  any tr (catMaybes [metaTitle meta, metaDescription meta]) || any itemTr (allItems items)
  where
    tr = not . M.null . i18nTranslations
    itemTr = \case
      ItemQuestion q -> tr (qLabel q) || any tr (qHint q) || any tr (choicesOf (qType q))
      ItemNote _ t _ -> tr t
      ItemGroup g -> any tr (groupTitle g)
      ItemRaw _ _ -> False
    choicesOf = \case
      QChoice _ _ cs -> map choiceLabel cs
      QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
      QRanking cs -> map choiceLabel cs
      QScale _ _ l h -> catMaybes [l, h]
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

writeItem :: Item -> Conv [Value]
writeItem = \case
  ItemNote i t rel -> do
    when (isJust rel) $ warn i "skip logic dropped, the SurveyMonkey API cannot set it"
    pure [heading (i18nDefault t) ["family" .= ("presentation" :: Text), "subtype" .= ("descriptive_text" :: Text)]]
  ItemRaw (Format "surveymonkey") v -> pure [v]
  ItemRaw (Format f) _ -> [] <$ warn "surveymonkey" ("raw " <> f <> " content dropped")
  ItemGroup g
    | Just (String st) <- M.lookup "surveymonkey:subtype" (groupAttrs g)
    , Just qs <- mapM asQuestion (groupItems g) -> do
        when (isJust (groupRelevant g)) $ warn (groupId g) "skip logic dropped, the SurveyMonkey API cannot set it"
        pure
          [ heading
              (maybe (groupId g) i18nDefault (groupTitle g))
              ( ["family" .= ("open_ended" :: Text), "subtype" .= st, "answers" .= object ["rows" .= [object ["text" .= i18nDefault (qLabel q)] | q <- qs]]]
                  ++ ["required" .= requiredJSON Nothing | any qRequired qs]
              )
          ]
    | otherwise -> do
        warn (groupId g) "group flattened into its page"
        when (isJust (groupRelevant g)) $ warn (groupId g) "skip logic dropped, the SurveyMonkey API cannot set it"
        concat <$> mapM writeItem (groupItems g)
  ItemQuestion q -> maybe [] pure <$> writeQuestion q
  where
    asQuestion = \case
      ItemQuestion q -> Just q
      _ -> Nothing

heading :: Text -> [(K.Key, Value)] -> Value
heading t rest = object (("headings" .= [object ["heading" .= t]]) : rest)

requiredJSON :: Maybe Value -> Value
requiredJSON t = object ["text" .= fromMaybe (String defaultRequired) t, "type" .= ("all" :: Text), "amount" .= ("0" :: Text)]

writeQuestion :: Question -> Conv (Maybe Value)
writeQuestion q = do
  when (isJust (qRelevant q)) $ warn (qId q) "skip logic dropped, the SurveyMonkey API cannot set it"
  when (isJust (qHint q)) $ warn (qId q) "hint dropped"
  when (isJust (qDefault q)) $ warn (qId q) "default value dropped"
  fields <- typeFields q
  pure $ flip fmap fields $ \fs ->
    heading (i18nDefault (qLabel q)) $
      fs
        ++ ["required" .= requiredJSON (M.lookup "surveymonkey:required_text" (qAttrs q)) | qRequired q]
        ++ [K.fromText k .= x | k <- carried, Just x <- [M.lookup ("surveymonkey:" <> k) (qAttrs q)]]

typeFields :: Question -> Conv (Maybe [(K.Key, Value)])
typeFields q = case qType q of
  QText k -> do
    when (k `elem` [Url, Phone]) $ warn (qId q) "url/phone format not enforced"
    val <- case (k, qValidation q) of
      (Email, _) -> pure [validation "email" "Please enter an email address." Nothing Nothing]
      (_, Just v) -> do
        exprDropped v
        pure [validation "text_length" (maybe defaultLength i18nDefault (valMessage v)) (valMin v) (valMax v) | isJust (valMin v) || isJust (valMax v)]
      _ -> pure []
    pure (Just (openEnded (if k == LongText then "essay" else "single") ++ val))
  QNumber k -> do
    let v = qValidation q
    mapM_ exprDropped v
    pure . Just $
      openEnded "single"
        ++ [ validation
               (if k == Integer then "integer" else "decimal")
               (maybe defaultNumber i18nDefault (v >>= valMessage))
               (v >>= valMin)
               (v >>= valMax)
           ]
  QChoice sel style cs -> do
    let others = [c | c <- cs, choiceOther c]
        regular = [c | c <- cs, not (choiceOther c)]
    when (length others > 1) $ warn (qId q) "only one other choice kept"
    unless (null [c | c <- regular, choiceExclusive c]) $ warn (qId q) "exclusive choice written as a plain choice"
    when (sel == Multiple && style == Dropdown) $ warn (qId q) "multi-select dropdown written as checkboxes"
    let (fam, sub) = case (sel, style) of
          (Single, Dropdown) -> ("single_choice", "menu")
          (Single, Buttons) -> ("single_choice", "horiz")
          (Single, Radio) -> ("single_choice", "vertical")
          (Multiple, Buttons) -> ("multiple_choice", "horiz")
          (Multiple, _) -> ("multiple_choice", "vertical")
    pure . Just $
      [ "family" .= (fam :: Text)
      , "subtype" .= (sub :: Text)
      , "answers"
          .= obj
            [ "choices" .=? toJSON [object ["text" .= i18nDefault (choiceLabel c)] | c <- regular]
            , case take 1 others of
                [o] -> "other" .=? toJSON [object ["text" .= i18nDefault (choiceLabel o), "num_lines" .= (1 :: Int), "num_chars" .= (50 :: Int), "is_answer_choice" .= True]]
                _ -> Nothing
            ]
      ]
  QMatrix sel rows cols -> do
    unless (null [c | c <- cols, choiceOther c || choiceExclusive c]) $ warn (qId q) "special matrix columns written as plain columns"
    let weights = mapM (readInt . choiceValue) cols
        positional = weights == Just [1 .. length cols]
        sub = case (sel, weights) of
          (Multiple, _) -> "multi"
          (Single, Just _) | not positional -> "rating"
          (Single, _) -> "single" :: Text
        colJSON = case (sub, weights) of
          ("rating", Just ws) -> [object ["text" .= i18nDefault (choiceLabel c), "weight" .= w] | (c, w) <- zip cols ws]
          _ -> [object ["text" .= i18nDefault (choiceLabel c)] | c <- cols]
    pure . Just $
      [ "family" .= ("matrix" :: Text)
      , "subtype" .= sub
      , "answers" .= object ["rows" .= [object ["text" .= i18nDefault (choiceLabel r)] | r <- rows], "choices" .= colJSON]
      ]
  QScale lo hi l h ->
    let text w lbl = maybe (T.pack (show w)) i18nDefault lbl
        cs = [object ["text" .= text w (if w == lo then l else if w == hi then h else Nothing), "weight" .= w] | w <- [lo .. hi]]
     in pure . Just $
          [ "family" .= ("matrix" :: Text)
          , "subtype" .= ("rating" :: Text)
          , "answers" .= object ["rows" .= [object ["text" .= ("" :: Text)]], "choices" .= cs]
          ]
  QRanking cs ->
    pure . Just $
      [ "family" .= ("matrix" :: Text)
      , "subtype" .= ("ranking" :: Text)
      , "answers" .= object ["rows" .= [object ["text" .= i18nDefault (choiceLabel c)] | c <- cs]]
      ]
  QDate k ->
    pure . Just $
      [ "family" .= ("datetime" :: Text)
      , "subtype" .= (case k of Date -> "date_only"; Time -> "time_only"; DateTime -> "both" :: Text)
      , "answers" .= object ["rows" .= [object ["text" .= ("" :: Text)]]]
      ]
  QBoolean -> do
    warn (qId q) "yes/no written as a two-choice question"
    pure . Just $
      [ "family" .= ("single_choice" :: Text)
      , "subtype" .= ("vertical" :: Text)
      , "answers" .= object ["choices" .= [object ["text" .= ("Yes" :: Text)], object ["text" .= ("No" :: Text)]]]
      ]
  QFile -> pure (Just (openEnded "single" ++ ["display_options" .= object ["display_type" .= ("file_upload" :: Text)]]))
  QGeo -> Just (openEnded "single") <$ warn (qId q) "geopoint written as a text box"
  QOther "demographic" ->
    pure . Just $
      [ "family" .= ("demographic" :: Text)
      , "subtype" .= fromMaybe (String "international") (M.lookup "surveymonkey:subtype" (qAttrs q))
      , "answers" .= fromMaybe (object ["rows" .= ([] :: [Value])]) (M.lookup "surveymonkey:answers" (qAttrs q))
      ]
  QOther t -> Nothing <$ warn (qId q) ("type " <> t <> " dropped")
  QCalculate _ -> Nothing <$ warn (qId q) "calculated value dropped"
  QHidden -> Nothing <$ warn (qId q) "hidden value dropped, set custom variables on the collector instead"
  where
    openEnded sub = ["family" .= ("open_ended" :: Text), "subtype" .= (sub :: Text)]
    validation t msg lo hi = "validation" .= object ["type" .= (t :: Text), "text" .= (msg :: Text), "min" .= fmap intOr lo, "max" .= fmap intOr hi]
    exprDropped v = when (isJust (valExpr v)) $ warn (qId q) "validation expression dropped"
    defaultLength = "The comment you entered is in an invalid format."
    readInt t = case reads (T.unpack t) :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing

-- | A whole number printed without a decimal point.
intOr :: Double -> Value
intOr d
  | d == fromIntegral r = toJSON r
  | otherwise = toJSON d
  where
    r = round d :: Integer

