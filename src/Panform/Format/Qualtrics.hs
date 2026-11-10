-- | Qualtrics Survey Format (QSF): a JSON export with a @SurveyEntry@ and a
-- list of @SurveyElements@ tagged by code: @BL@ (blocks), @FL@ (survey
-- flow), @SQ@ (one per question), @SO@ (options, including languages) and
-- bookkeeping elements. Blocks hold content, the flow holds order and
-- branching; logic is a JSON tree of typed conditions over @q://QID@
-- locators rather than a text expression language.
module Panform.Format.Qualtrics
  ( readQualtrics
  , writeQualtrics
  , stripHtml
  , selectorFor
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Writer.Strict (censor)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Numeric (showFFloat)
import Panform.AST
import Panform.Conv
import Panform.Json

------------------------------------------------------------------------------
-- Shared helpers

-- | Object members (or array entries) in index order: @"2"@ before @"10"@.
indexed :: Value -> [(Text, Value)]
indexed = \case
  Object o -> sortOn (ord . fst) [(K.toText k, v) | (k, v) <- KM.toList o]
  Array a -> zip (map tshow [0 :: Int ..]) (V.toList a)
  _ -> []
  where
    ord k = maybe (1 :: Int, 0, k) (\n -> (0, n, k)) (readNat k)

readNat :: Text -> Maybe Int
readNat t = case TR.decimal t of
  Right (n, "") -> Just n
  _ -> Nothing

isIndex :: Text -> Bool
isIndex = isJust . readNat

tshow :: Show a => a -> Text
tshow = T.pack . show

numText :: Double -> Text
numText n
  | n == fromIntegral r = tshow r
  | otherwise = T.pack (showFFloat Nothing n "")
  where
    r = round n :: Integer

andE :: Maybe Expr -> Maybe Expr -> Maybe Expr
andE (Just a) (Just b) = Just (EBin And a b)
andE a b = a <|> b

htmlWarning :: Text
htmlWarning = "html markup removed from question text"

-- | Plain text from Qualtrics rich text. The flag says whether markup other
-- than line breaks was removed; @<br>@ and entities alone are how plain
-- text is written, so they do not count.
stripHtml :: Text -> (Text, Bool)
stripHtml src
  | not rich = (decode (breaks src), False)
  | otherwise = (tidy (decode (go (T.unwords (T.words src)))), True)
  where
    tags = [T.toLower (T.takeWhile (\c -> isAlphaNum c || c == '/') t) | t <- drop 1 (T.splitOn "<" src)]
    rich = any (\t -> not (T.null t) && t `notElem` ["br", "br/"]) tags
    go t = case T.breakOn "<" t of
      (pre, rest)
        | T.null rest -> pre
        | otherwise ->
            let (tag, after) = T.breakOn ">" rest
                name = T.toLower (T.takeWhile (\c -> isAlphaNum c || c == '/') (T.drop 1 tag))
                isTag = not (T.null name) || "<!" `T.isPrefixOf` tag
                brk = name `elem` ["br", "br/", "/p", "/div", "/li", "/h1", "/h2", "/h3", "/h4", "/tr"]
             in if isTag && not (T.null after)
                  then pre <> (if brk then "\n" else "") <> go (T.drop 1 after)
                  else pre <> "<" <> go (T.drop 1 rest)
    -- every spelling of a line break (@<br>@, @<br/>@, @<br />@, @<BR>@);
    -- nothing else is touched
    breaks t = case T.breakOn "<" t of
      (pre, rest)
        | T.null rest -> pre
        | otherwise ->
            let (tag, after) = T.breakOn ">" rest
                name = T.toLower (T.takeWhile (\c -> isAlphaNum c || c == '/') (T.drop 1 tag))
             in if name `elem` ["br", "br/"] && not (T.null after)
                  then pre <> "\n" <> breaks (T.drop 1 after)
                  else pre <> "<" <> breaks (T.drop 1 rest)
    tidy = T.intercalate "\n" . squeeze . map (T.unwords . T.words) . T.lines . T.strip
    squeeze (a : b : c : rest) | T.null a && T.null b = squeeze (b : c : rest)
    squeeze (x : rest) = x : squeeze rest
    squeeze [] = []

-- | Decode the entities Qualtrics exports use.
decode :: Text -> Text
decode t = case T.breakOn "&" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let (ent, after) = T.breakOn ";" (T.drop 1 rest)
         in case entity ent of
              Just c | not (T.null after), T.length ent <= 8 -> pre <> T.singleton c <> decode (T.drop 1 after)
              _
                | Just more <- T.stripPrefix "&nbsp" rest -> pre <> " " <> decode more
                | otherwise -> pre <> "&" <> decode (T.drop 1 rest)
  where
    entity e = case e of
      "nbsp" -> Just ' '
      "hellip" -> Just '\x2026'
      "euro" -> Just '\x20AC'
      "bull" -> Just '\x2022'
      "amp" -> Just '&'
      "lt" -> Just '<'
      "gt" -> Just '>'
      "quot" -> Just '"'
      "apos" -> Just '\''
      "rsquo" -> Just '\x2019'
      "lsquo" -> Just '\x2018'
      "rdquo" -> Just '\x201D'
      "ldquo" -> Just '\x201C'
      "ndash" -> Just '\x2013'
      "mdash" -> Just '\x2014'
      _ | Just c <- lookup e latin1 -> Just c
      _ -> case T.unpack e of
        ('#' : x : hex) | x `elem` ("xX" :: String) -> case TR.hexadecimal (T.pack hex) of
          Right (n, "") -> scalar n
          _ -> Nothing
        ('#' : dec) -> readNat (T.pack dec) >>= scalar
        _ -> Nothing
    -- A numeric reference names a character only inside the Unicode scalar
    -- range; anything else (out of range, a surrogate, trailing garbage)
    -- is not an entity and stays in the text as written.
    scalar :: Int -> Maybe Char
    scalar n
      | n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF) = Nothing
      | otherwise = Just (toEnum n)

-- | The HTML 4 Latin-1 entities, U+00A0 to U+00FF in order.
latin1 :: [(Text, Char)]
latin1 = zip (T.words names) ['\xA0' ..]
  where
    names =
      "nbsp iexcl cent pound curren yen brvbar sect uml copy ordf laquo not shy reg macr \
      \deg plusmn sup2 sup3 acute micro para middot cedil sup1 ordm raquo frac14 frac12 frac34 iquest \
      \Agrave Aacute Acirc Atilde Auml Aring AElig Ccedil Egrave Eacute Ecirc Euml Igrave Iacute Icirc Iuml \
      \ETH Ntilde Ograve Oacute Ocirc Otilde Ouml times Oslash Ugrave Uacute Ucirc Uuml Yacute THORN szlig \
      \agrave aacute acirc atilde auml aring aelig ccedil egrave eacute ecirc euml igrave iacute icirc iuml \
      \eth ntilde ograve oacute ocirc otilde ouml divide oslash ugrave uacute ucirc uuml yacute thorn yuml"

escapeHtml :: Text -> Text
escapeHtml = T.replace "\n" "<br>" . T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"

-- | The MC selector a choice question gets when nothing asks otherwise.
selectorFor :: Selection -> ChoiceStyle -> Text
selectorFor sel style = case (sel, style) of
  (Single, Dropdown) -> "DL"
  (Multiple, Dropdown) -> "MSB"
  (Single, _) -> "SAVR"
  (Multiple, _) -> "MAVR"

------------------------------------------------------------------------------
-- Reading

-- | What logic needs to know about a question: export tag, kind, and
-- choice id to coded value.
data Kind = KChoice Selection | KNps | KSlider | KText | KHidden | KNone
  deriving (Eq)

data Info = Info Text Kind (M.Map Text Text)

readQualtrics :: BL.ByteString -> Conv Form
readQualtrics bs = censor onceHtml $ do
  v <- eitherDecode bs `orFail` "Qualtrics QSF JSON"
  let entry = fromMaybe Null (key "SurveyEntry" v)
      els = keyArr "SurveyElements" v
      element c = [e | e <- els, keyText "Element" e == Just c]
      payloadOf c = listToMaybe (element c) >>= key "Payload"
      sqs = [(fromMaybe "" (keyText "QuestionID" p <|> keyText "PrimaryAttribute" e), p) | e <- element "SQ", Just p <- [key "Payload" e]]
      dl = T.toLower (keyTextOr "EN" "SurveyLanguage" entry)
      avail = [T.toLower l | (l, _) <- indexed (fromMaybe Null (payloadOf "SO" >>= key "AvailableLanguages")), T.toLower l /= dl]
      env = M.fromList [(qid, info qid p) | (qid, p) <- sqs]
      sqMap = M.fromList sqs
      blocks = maybe [] (map snd . indexed) (payloadOf "BL")
      blockMap = M.fromList [(i, b) | b <- blocks, Just i <- [keyText "ID" b]]
      flow = maybe [] (keyArr' "Flow") (payloadOf "FL")
      inFlow = S.fromList (flowBlocks flow)
  when (null els) $ failWith "Qualtrics QSF: no SurveyElements"
  forM_ [b | b <- blocks, keyText "Type" b == Just "Trash", not (null (keyArr' "BlockElements" b))] $ \_ ->
    warn "qualtrics" "trash block skipped"
  forM_ [c | e <- els, Just c <- [keyText "Element" e], c `notElem` known] $ \c ->
    warn "qualtrics" ("survey element " <> c <> " dropped")
  let ctx = Ctx env sqMap blockMap dl
      -- translations present on questions but missing from SO's list
      used = [l | (_, p) <- sqs, (l, _) <- langObjs ctx p]
  flowItems <- readFlow ctx True Nothing flow
  let orphans = [b | b <- blocks, keyText "Type" b /= Just "Trash", Just i <- [keyText "ID" b], not (S.member i inFlow)]
  orphanItems <- fmap concat . forM orphans $ \b -> do
    warn (keyTextOr "" "ID" b) "block not in the survey flow, appended at the end"
    readBlock ctx Nothing b
  let so = fromMaybe Null (payloadOf "SO")
      title = keyText "SurveyName" entry >>= \t -> if T.null t then Nothing else Just t
      meta =
        emptyMeta
          { metaId = keyText "SurveyID" entry
          , metaTitle = plain <$> title
          , metaDescription = plain <$> keyText "SurveyDescription" entry
          , metaLanguages = dl : nub (avail ++ S.toList (S.fromList used))
          , metaAttrs =
              M.fromList $
                [("qualtrics:SurveyTitle", String t) | Just t <- [keyText "SurveyTitle" so], Just t /= title]
                  ++ [("qualtrics:" <> k, String t) | (k, d) <- keptOptions, Just t <- [keyText k so], t /= d]
          }
  -- the password itself is not carried into other formats
  when (keyText "PasswordProtection" so `notElem` [Nothing, Just "No"] || maybe False (not . T.null) (keyText "Password" so)) $
    warn "qualtrics" "survey password protection dropped"
  pure (Form meta (flowItems ++ orphanItems))
  where
    known = ["BL", "FL", "SQ", "SO", "QC", "RS", "SCO", "STAT", "PROJ", "PL"]
    onceHtml ws = let (h, rest) = span' ws in take 1 h ++ rest
    span' ws = ([w | w <- ws, warnWhat w == htmlWarning], [w | w <- ws, warnWhat w /= htmlWarning])

-- | Survey options that decide who may respond and what is recorded about
-- them, with the value the writer uses when the survey sets none. A
-- survey's own value is kept (as a @qualtrics:@ attribute) and written
-- back, so a conversion never makes a survey more open than it was.
keptOptions :: [(Text, Text)]
keptOptions =
  [ ("SurveyProtection", "PublicSurvey")
  , ("AnonymizeResponse", "No")
  , ("BallotBoxStuffingPrevention", "false")
  , ("RefererCheck", "No")
  , ("RefererURL", "http://")
  ]

-- | Like 'keyArr' but in index order for index-keyed objects.
keyArr' :: Text -> Value -> [Value]
keyArr' k v = maybe [] (map snd . indexed) (key k v)

info :: Text -> Value -> Info
info qid p = Info (fromMaybe qid (keyText "DataExportTag" p)) kind vals
  where
    sel = keyTextOr "" "Selector" p
    kind = case keyTextOr "" "QuestionType" p of
      "MC"
        | sel == "NPS" -> KNps
        | "MA" `T.isPrefixOf` sel || sel == "MSB" -> KChoice Multiple
        | otherwise -> KChoice Single
      "TE" | sel /= "FORM" -> KText
      "Slider" -> KSlider
      _ -> KNone
    recodes = choiceCodes p
    vals = M.fromList [(k, M.findWithDefault k k recodes) | (k, _) <- indexed (fromMaybe Null (key "Choices" p))]

-- | Coded values by choice id: recode values, else variable naming (how
-- text codes are written).
choiceCodes :: Value -> M.Map Text Text
choiceCodes p = M.union (field "RecodeValues") (field "VariableNaming")
  where
    field f = M.fromList [(k, t) | (k, x) <- indexed (fromMaybe Null (key f p)), Just t <- [asText x], f == "VariableNaming" || not (T.null t)]

data Ctx = Ctx
  { cEnv :: M.Map Text Info
  , cSQ :: M.Map Text Value
  , cBlocks :: M.Map Text Value
  , cLang :: Lang
  }

flowBlocks :: [Value] -> [Text]
flowBlocks = concatMap $ \f ->
  [i | keyText "Type" f `elem` map Just ["Block", "Standard"], Just i <- [keyText "ID" f]] ++ flowBlocks (keyArr' "Flow" f)

readFlow :: Ctx -> Bool -> Maybe Expr -> [Value] -> Conv [Item]
readFlow ctx top rel fs = fmap concat . forM (zip [1 :: Int ..] fs) $ \(n, f) -> do
  let fid = keyTextOr "flow" "FlowID" f
  case keyTextOr "" "Type" f of
    t | t `elem` ["Block", "Standard"] -> case keyText "ID" f >>= (`M.lookup` cBlocks ctx) of
      Just b -> readBlock ctx rel b
      Nothing -> [] <$ warn fid "flow names a block that does not exist"
    "Branch" -> do
      cond <- maybe (pure Nothing) (readLogic ctx fid) (key "BranchLogic" f)
      readFlow ctx False (andE rel cond) (keyArr' "Flow" f)
    "EmbeddedData" ->
      pure
        [ ItemQuestion (question field field QHidden) {qRelevant = rel, qDefault = keyText "Value" ed >>= nonEmpty}
        | ed <- keyArr' "EmbeddedData" f
        , Just field <- [keyText "Field" ed]
        ]
    "EndSurvey"
      | top && n == length fs -> pure []
      | otherwise -> [] <$ warn fid "end-of-survey element dropped (screen-outs are not represented)"
    t | t `elem` ["BlockRandomizer", "Randomizer", "Group"] -> do
      unless (t == "Group") $ warn fid "randomizer dropped, its elements kept in order"
      readFlow ctx top rel (keyArr' "Flow" f)
    t -> do
      warn fid ("flow element " <> t <> " dropped")
      readFlow ctx False rel (keyArr' "Flow" f)
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- | A block becomes one page per page break; later pages remember the
-- block they continue so a Qualtrics writer can rejoin them.
readBlock :: Ctx -> Maybe Expr -> Value -> Conv [Item]
readBlock ctx rel b = do
  let rawId = keyTextOr "block" "ID" b
      bid = fromMaybe rawId (T.stripPrefix "BL_" rawId)
      opts = fromMaybe Null (key "Options" b)
      pages = case splitPages (keyArr' "BlockElements" b) of
        (p : ps) -> p : filter (not . null) ps
        [] -> [[]]
  when (keyTextOr "false" "RandomizeQuestions" opts `notElem` ["false", ""]) $ warn rawId "question randomization dropped"
  when (isJust (key "Looping" opts) && keyText "Looping" opts /= Just "None") $ warn bid "loop and merge dropped"
  forM (zip [1 :: Int ..] pages) $ \(n, els) -> do
    items <- concat <$> mapM (readElement ctx) els
    pure . ItemGroup $
      Group
        { groupId = if n == 1 then bid else bid <> "-" <> tshow n
        , groupKind = Page
        , groupTitle = if n == 1 then plain <$> keyText "Description" b else Nothing
        , groupRelevant = rel
        , groupItems = items
        , groupAttrs = if n == 1 then M.empty else M.singleton "qualtrics:block" (String bid)
        }
  where
    splitPages es = case break ((== Just "Page Break") . keyText "Type") es of
      (pre, []) -> [pre]
      (pre, _ : rest) -> pre : splitPages rest

readElement :: Ctx -> Value -> Conv [Item]
readElement ctx e = case keyText "Type" e of
  Just "Question" -> case keyText "QuestionID" e of
    Just qid -> case M.lookup qid (cSQ ctx) of
      Just p -> readSQ ctx qid p
      Nothing -> [] <$ warn qid "block names a question that does not exist"
    Nothing -> pure []
  Just t -> [] <$ warn "qualtrics" ("block element " <> t <> " dropped")
  Nothing -> pure []

-- | Rich text into plain text, flagging removed markup once per survey.
clean :: Text -> Conv Text
clean t = do
  let (p, rich) = stripHtml t
  when rich $ warn "qualtrics" htmlWarning
  pure p

-- | A payload field in the default language plus its translations from the
-- question's @Language@ object.
i18nAt :: Ctx -> Value -> [Text] -> Conv (Maybe I18n)
i18nAt ctx p path = case lookupPath path p >>= asText of
  Nothing -> pure Nothing
  Just d -> do
    d' <- clean d
    trs <- forM (langObjs ctx p) $ \(l, o) -> case lookupPath path o >>= asText of
      Just t -> Just . (,) l <$> clean t
      Nothing -> pure Nothing
    pure (Just (I18n d' (M.fromList [(l, t) | Just (l, t) <- trs, t /= d', not (T.null t)])))

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath ks v = foldl (\acc k -> acc >>= key k) (Just v) ks

langObjs :: Ctx -> Value -> [(Lang, Value)]
langObjs ctx p = case key "Language" p of
  Just o@(Object _) -> [(T.toLower l, x) | (l, x) <- indexed o, T.toLower l /= cLang ctx]
  _ -> []

readChoices :: Ctx -> Value -> Text -> Text -> Bool -> Conv [Choice]
readChoices ctx p field orderField recode = do
  let cs = fromMaybe Null (key field p)
      ids = case mapMaybe asText (keyArr orderField p) of
        [] -> map fst (indexed cs)
        o -> o
      recodes = if recode then choiceCodes p else M.empty
  fmap catMaybes . forM ids $ \i -> case key i cs of
    Nothing -> pure Nothing
    Just c -> do
      lbl <- fromMaybe (plain "") <$> i18nAt ctx p [field, i, "Display"]
      when (isJust (key "DisplayLogic" c)) $ warn (keyTextOr "" "QuestionID" p) ("display logic on choice " <> i <> " dropped")
      pure . Just $
        Choice
          { choiceValue = M.findWithDefault i i recodes
          , choiceLabel = lbl
          , choiceOther = keyBool "TextEntry" c == Just True || keyText "TextEntry" c == Just "on"
          , choiceExclusive = keyBool "ExclusiveAnswer" c == Just True
          }

readSQ :: Ctx -> Text -> Value -> Conv [Item]
readSQ ctx qid p = do
  let Info tag _ _ = maybe (Info qid KNone M.empty) id (M.lookup qid (cEnv ctx))
      qt = keyTextOr "" "QuestionType" p
      sel = keyTextOr "" "Selector" p
      sub = keyTextOr "" "SubSelector" p
      settings = fromMaybe Null (key "Validation" p >>= key "Settings")
      force = keyTextOr "OFF" "ForceResponse" settings
      html = keyTextOr "" "QuestionText" p
  label <- fromMaybe (plain "") <$> i18nAt ctx p ["QuestionText"]
  rel <- maybe (pure Nothing) (readLogic ctx tag) (key "DisplayLogic" p)
  when (isJust (key "SkipLogic" p)) $ warn tag "skip logic dropped"
  when (maybe False (\r -> keyTextOr "None" "Type" r /= "None") (key "Randomization" p)) $ warn tag "choice randomization dropped"
  -- choices carried forward from another question are not represented;
  -- the question keeps only the choices written on it
  forM_ (key "DynamicChoices" p) $ \d ->
    warn tag ("dynamic choices dropped (carried forward from " <> keyTextOr "another question" "Locator" d <> ")")
  -- a text entry question's validation is read by 'textType'; any other
  -- question's (how many choices to pick, a custom rule) is not represented
  -- unless the question is kept whole
  let noValidation =
        forM_ [t | Just t <- [keyText "Type" settings], t `notElem` ["None", ""]] $ \t ->
          warn tag ("validation " <> t <> " dropped")
  let base =
        (question tag "" (QOther qt))
          { qLabel = label
          , qRequired = force == "ON"
          , qRelevant = rel
          , qAttrs =
              M.fromList $
                [("qualtrics:ForceResponse", String force) | force `notElem` ["ON", "OFF"]]
                  ++ [("qualtrics:html", String html) | snd (stripHtml html)]
          }
      withSel q = q {qAttrs = M.insert "qualtrics:selector" (String sel) (qAttrs q)}
      other name = do
        warn tag ("unsupported question type " <> name <> " kept by name")
        pure [ItemQuestion base {qType = QOther name, qAttrs = M.insert "qualtrics:payload" p (qAttrs base)}]
      one t = pure [ItemQuestion base {qType = t}]
  case qt of
    "DB" -> pure [ItemNote tag label rel]
    "MC" -> do
      noValidation
      cs <- readChoices ctx p "Choices" "ChoiceOrder" True
      case sel of
        "NPS" -> pure [ItemQuestion (withSel base) {qType = QScale 0 10 Nothing Nothing}]
        _ -> do
          let multi = "MA" `T.isPrefixOf` sel || sel == "MSB"
              s = if multi then Multiple else Single
              style = if sel `elem` ["DL", "SB", "MSB"] then Dropdown else Radio
              q = base {qType = QChoice s style cs}
          unless (sel `elem` ["SAVR", "SAHR", "SACOL", "DL", "SB", "MAVR", "MAHR", "MACOL", "MSB"]) $
            warn tag ("multiple choice selector " <> sel <> " read as a plain choice")
          pure [ItemQuestion (if selectorFor s style == sel then q else withSel q)]
    "TE"
      | sel == "FORM" -> do
          fields <- readChoices ctx p "Choices" "ChoiceOrder" False
          warn tag "form expanded into one text question per field"
          pure
            [ ItemGroup $
                Group
                  { groupId = tag
                  , groupKind = Section
                  , groupTitle = Just label
                  , groupRelevant = rel
                  , groupItems =
                      [ ItemQuestion (question (tag <> "_" <> choiceValue c) "" (QText ShortText)) {qLabel = choiceLabel c, qRequired = qRequired base}
                      | c <- fields
                      ]
                  , groupAttrs = M.singleton "qualtrics:form" (Bool True)
                  }
            ]
      | otherwise -> do
          (t, v) <- textType tag sel settings
          pure [ItemQuestion (if sel == "PW" then withSel base else base) {qType = t, qValidation = v}]
    "Matrix"
      | sel `elem` ["Likert", "Bipolar"] && sub `elem` ["SingleAnswer", "MultipleAnswer", ""] -> do
          noValidation
          rows <- readChoices ctx p "Choices" "ChoiceOrder" False
          cols <- readChoices ctx p "Answers" "AnswerOrder" True
          let rowTags = M.fromList [(k, t) | (k, x) <- indexed (fromMaybe Null (key "ChoiceDataExportTags" p)), Just t <- [asText x]]
              rows' = [r {choiceValue = M.findWithDefault (choiceValue r) (choiceValue r) rowTags} | r <- rows]
          when (sel == "Bipolar") $ warn tag "bipolar matrix read as a plain matrix"
          one (QMatrix (if sub == "MultipleAnswer" then Multiple else Single) rows' cols)
      | otherwise -> other ("Matrix:" <> sel)
    "Slider"
      | length (keyArr "Choices" p) <= 1 -> do
          noValidation
          let cfg = fromMaybe Null (key "Configuration" p)
              labels = keyArr' "Labels" p
          ls <- mapM (\l -> traverse clean (keyText "Display" l)) labels
          unless (sel == "HSLIDER") $ warn tag ("slider style " <> sel <> " read as a scale")
          pure
            [ ItemQuestion (if sel == "HSLIDER" then base else withSel base)
                { qType =
                    QScale
                      (maybe 0 round (keyNum "CSSliderMin" cfg))
                      (maybe 100 round (keyNum "CSSliderMax" cfg))
                      (plain <$> (listToMaybe ls >>= id))
                      (plain <$> (if length ls > 1 then last ls else Nothing))
                }
            ]
      | otherwise -> other "Slider"
    "RO" -> do
      noValidation
      cs <- readChoices ctx p "Choices" "ChoiceOrder" True
      pure [ItemQuestion (if sel == "DND" then base else withSel base) {qType = QRanking cs}]
    "FileUpload" -> noValidation >> one QFile
    _ -> other (if T.null sel then qt else qt <> ":" <> sel)

-- | A text entry question's type from its content-type validation.
textType :: Text -> Text -> Value -> Conv (QType, Maybe Validation)
textType tag sel settings = do
  let long = QText (if sel `elem` ["ML", "ESTB"] then LongText else ShortText)
  case keyTextOr "None" "Type" settings of
    "None" -> pure (long, Nothing)
    "ContentType" -> case keyTextOr "" "ContentType" settings of
      "ValidNumber" -> do
        let n = fromMaybe Null (key "ValidNumber" settings)
            lo = keyNum "Min" n
            hi = keyNum "Max" n
        pure (QNumber Decimal, if isJust lo || isJust hi then Just (Validation lo hi Nothing Nothing) else Nothing)
      "ValidEmail" -> pure (QText Email, Nothing)
      c | c `elem` ["ValidPhone", "ValidUSPhone"] -> pure (QText Phone, Nothing)
      "ValidDate" -> pure (QDate Date, Nothing)
      c -> (long, Nothing) <$ warn tag ("content validation " <> c <> " dropped")
    t -> (long, Nothing) <$ warn tag ("validation " <> t <> " dropped")

------------------------------------------------------------------------------
-- Logic

-- | A DisplayLogic / BranchLogic tree. Groups keyed "0", "1", ... are
-- joined by Or; within a group each condition after the first carries a
-- @Conjuction@ (sic), with And binding tighter than Or.
readLogic :: Ctx -> Text -> Value -> Conv (Maybe Expr)
readLogic ctx at v = do
  groups <- forM [(g) | (k, g) <- indexed v, isIndex k] $ \g -> do
    conds <- forM [c | (k, c) <- indexed g, isIndex k] $ \c -> (,) (keyTextOr "And" "Conjuction" c) <$> readCond ctx at c
    pure ((keyTextOr "Or" "Conjuction" g), chain conds)
  pure (chain [(c, e) | (c, Just e) <- groups])
  where
    chain :: [(Text, Expr)] -> Maybe Expr
    chain [] = Nothing
    chain ((_, e0) : rest) = Just (foldl1 (EBin Or) (map (foldl1 (EBin And)) (runs [e0] rest)))
    runs acc [] = [reverse acc]
    runs acc ((c, e) : rest)
      | c == "Or" = reverse acc : runs [e] rest
      | otherwise = runs (e : acc) rest

readCond :: Ctx -> Text -> Value -> Conv Expr
readCond ctx at c = case condExpr ctx c of
  Just e -> pure e
  Nothing -> do
    let raw = rawCond c
    warn at ("logic condition kept in qualtrics form: " <> raw)
    pure (ERaw "qualtrics" raw)

-- | A condition as compact JSON, without its display description.
rawCond :: Value -> Text
rawCond = \case
  Object o -> enc (Object (KM.delete "Description" (KM.delete "Conjuction" o)))
  v -> enc v
  where
    enc = TL.toStrict . TLE.decodeUtf8 . encode

condExpr :: Ctx -> Value -> Maybe Expr
condExpr ctx c = do
  op <- keyText "Operator" c
  let rhs = fromMaybe "" (keyText "RightOperand" c)
  case keyText "LogicType" c of
    Just "Question" -> do
      loc <- keyText "LeftOperand" c <|> keyText "ChoiceLocator" c
      (qid, rest) <- case T.splitOn "/" <$> T.stripPrefix "q://" loc of
        Just (q : r) -> Just (q, r)
        _ -> Nothing
      Info tag kind vals <- M.lookup qid (cEnv ctx)
      case (rest, kind) of
        (["SelectableChoice", cid], KChoice sel) -> do
          v <- M.lookup cid vals
          case (op, sel) of
            ("Selected", Multiple) -> Just (ESelected tag v)
            ("NotSelected", Multiple) -> Just (ENot (ESelected tag v))
            ("Selected", Single) -> Just (EBin Eq (ERef tag) (ELit (LStr v)))
            ("NotSelected", Single) -> Just (EBin Neq (ERef tag) (ELit (LStr v)))
            _ -> Nothing
        (["SelectableChoice", cid], KNps) -> do
          v <- M.lookup cid vals
          case op of
            "Selected" -> Just (EBin Eq (ERef tag) (lit v))
            "NotSelected" -> Just (EBin Neq (ERef tag) (lit v))
            _ -> Nothing
        -- the writer tests an NPS question the same way as a choice
        (["SelectedChoicesCount"], k) | k == KNps || isChoiceKind k -> case (op, rhs) of
          ("GreaterThan", "0") -> Just (EAnswered tag)
          ("EqualTo", "0") -> Just (ENot (EAnswered tag))
          ("LessThanOrEqual", "0") -> Just (ENot (EAnswered tag))
          _ -> Nothing
        (["ChoiceTextEntryValue"], KText) -> compareOp op tag rhs
        (["ChoiceNumericEntryValue", _], KSlider) -> compareOp op tag rhs
        _ -> Nothing
    Just "EmbeddedField" -> do
      l <- keyText "LeftOperand" c
      compareOp op (fromMaybe l (T.stripPrefix "e://Field/" l)) rhs
    _ -> Nothing
  where
    isChoiceKind = \case
      KChoice _ -> True
      _ -> False

compareOp :: Text -> Text -> Text -> Maybe Expr
compareOp op tag rhs = case op of
  "Empty" -> Just (ENot (EAnswered tag))
  "NotEmpty" -> Just (EAnswered tag)
  _ -> (\o -> EBin o (ERef tag) (lit rhs)) <$> lookup op opNames

lit :: Text -> Expr
lit t = case TR.signed TR.rational t of
  Right (d, "") | numText d == t -> ELit (LNum d)
  _ -> ELit (LStr t)

opNames :: [(Text, BinOp)]
opNames =
  [ ("EqualTo", Eq)
  , ("NotEqualTo", Neq)
  , ("GreaterThan", Gt)
  , ("GreaterThanOrEqual", Ge)
  , ("LessThan", Lt)
  , ("LessThanOrEqual", Le)
  ]

------------------------------------------------------------------------------
-- Writing

-- | What a block holds, in order.
data El
  = ElQ Question
  | ElNote Text I18n (Maybe Expr)
  | ElForm Group
  | ElBreak

data Unit
  = UBlock Text (Maybe I18n) (Maybe Expr) [El]
  -- ^ Block id, title, relevance (a flow branch), content.
  | UEmbedded (Maybe Expr) [Question]

-- | Top-level items into flow units: pages become blocks (continuation
-- pages rejoin their block), loose content an automatic block, hidden
-- values embedded-data elements.
planFlow :: [Item] -> Conv [Unit]
planFlow = go (1 :: Int) []
  where
    go _ acc [] = pure (mergeEmbedded (reverse acc))
    go n acc (item : rest) = case item of
      ItemGroup g -> do
        unless (groupKind g == Page) $ warn (groupId g) "top-level group written as a block"
        (hidden, els) <- flatten Nothing (groupItems g)
        let emb = embeddedRuns hidden
            cont = M.lookup "qualtrics:block" (groupAttrs g)
        case acc of
          UBlock bid t r prev : acc'
            | cont == Just (String bid) && r == groupRelevant g ->
                go n (reverse emb ++ UBlock bid t r (prev ++ ElBreak : els) : acc') rest
          _ -> go n (UBlock (groupId g) (groupTitle g) (groupRelevant g) els : reverse emb ++ acc) rest
      ItemQuestion q | isHidden q -> case acc of
        UEmbedded r qs : acc' | r == qRelevant q -> go n (UEmbedded r (qs ++ [q]) : acc') rest
        _ -> go n (UEmbedded (qRelevant q) [q] : acc) rest
      ItemRaw (Format f) _ -> warn "qualtrics" ("raw " <> f <> " content dropped") >> go n acc rest
      _ -> do
        let (loose, rest') = break isBoundary (item : rest)
        (hidden, els) <- flatten Nothing loose
        let bid = "BL_auto" <> tshow n
        go (n + 1) (UBlock bid (Just (plain ("Block " <> tshow n))) Nothing els : reverse (embeddedRuns hidden) ++ acc) rest'
    isBoundary = \case
      ItemGroup _ -> True
      ItemQuestion q -> isHidden q
      ItemRaw _ _ -> True
      _ -> False

-- | Consecutive hidden values with the same condition share one
-- embedded-data element, as the reader groups them.
embeddedRuns :: [(Maybe Expr, [Question])] -> [Unit]
embeddedRuns = foldr add []
  where
    add (r, qs) (UEmbedded r' qs' : rest) | r == r' = UEmbedded r (qs ++ qs') : rest
    add (r, qs) rest = UEmbedded r qs : rest

-- | Adjacent embedded-data units with one condition read back as one.
mergeEmbedded :: [Unit] -> [Unit]
mergeEmbedded = \case
  UEmbedded r qs : UEmbedded r' qs' : rest | r == r' -> mergeEmbedded (UEmbedded r (qs ++ qs') : rest)
  u : rest -> u : mergeEmbedded rest
  [] -> []

isHidden :: Question -> Bool
isHidden q = case qType q of
  QHidden -> True
  QCalculate _ -> True
  _ -> False

-- | A block's content without nesting; nested group relevance moves onto
-- each child, hidden values come out separately.
flatten :: Maybe Expr -> [Item] -> Conv ([(Maybe Expr, [Question])], [El])
flatten rel items = do
  parts <- forM items $ \case
    ItemQuestion q
      | isHidden q -> pure ([(andE rel (qRelevant q), [q])], [])
      | otherwise -> pure ([], [ElQ q {qRelevant = andE rel (qRelevant q)}])
    ItemNote i t r -> pure ([], [ElNote i t (andE rel r)])
    ItemGroup g
      | M.lookup "qualtrics:form" (groupAttrs g) == Just (Bool True)
      , all isTextQ (groupItems g) ->
          pure ([], [ElForm g {groupRelevant = andE rel (groupRelevant g)}])
      | otherwise -> do
          warn (groupId g) $ case groupKind g of
            Repeat -> "repeat written as plain questions, it is asked once"
            _ -> "nested group flattened into its block"
          when (isJust (groupTitle g)) $ warn (groupId g) "nested group title dropped"
          flatten (andE rel (groupRelevant g)) (groupItems g)
    ItemRaw (Format f) _ -> ([], []) <$ warn "qualtrics" ("raw " <> f <> " content dropped")
  pure (concatMap fst parts, concatMap snd parts)
  where
    isTextQ = \case
      ItemQuestion q -> qType q == QText ShortText
      _ -> False

-- | What the writer's logic needs per referenced id.
data WInfo = WInfo Text Kind (M.Map Text Text)

writeQualtrics :: Form -> Conv BL.ByteString
writeQualtrics (Form meta items) = do
  units0 <- planFlow items
  let surveyId = case metaId meta of
        Just i | "SV_" `T.isPrefixOf` i -> i
        _ -> "SV_panform"
      contents = concat [els | UBlock _ _ _ els <- units0]
      entries = [e | e <- contents, not (isBreak e)]
      qids = ["QID" <> tshow n | (n, _) <- zip [1 :: Int ..] entries]
      hiddenQs = concat [qs | UEmbedded _ qs <- units0]
      env =
        M.fromList $
          [(elTag e, WInfo qid (elKind e) (elVals e)) | (e, qid) <- zip entries qids]
            ++ [(qId q, WInfo "" KHidden M.empty) | q <- hiddenQs]
      dl = case metaLanguages meta of
        (l : _) -> l
        [] -> "en"
      langs = filter (/= dl) (nub (drop 1 (metaLanguages meta) ++ S.toList (S.fromList (concatMap elLangs entries))))
  -- a condition the flow cannot carry is dropped here, once, so the
  -- embedded data it guarded joins its neighbours as it will on reading
  units <- fmap mergeEmbedded . forM units0 $ \case
    UEmbedded r@(Just _) qs -> do
      l <- writeLogic env "embedded data" False r
      pure (UEmbedded (if isJust l then r else Nothing) qs)
    u -> pure u
  forM_ [qId q | q <- hiddenQs, QCalculate _ <- [qType q]] $ \i -> warn i "calculation dropped, kept as an embedded data field"
  forM_ [q | q <- hiddenQs, qLabel q /= plain (qId q)] $ \q -> warn (qId q) "embedded data field label dropped"
  forM_ hiddenQs $ \q -> warnQidRefs (qId q) (map plain (maybe [] pure (qDefault q))) (qRelevant q)
  when (maybe False (not . M.null . i18nTranslations) (metaTitle meta)) $ warn "title" "title translations dropped"
  sqs <- forM (zip entries qids) $ \(e, qid) -> writeEl env langs surveyId qid e
  let blockIds = uniqueIds [bid | UBlock bid _ _ _ <- units]
  forM_ (zip [bid | UBlock bid _ _ _ <- units] blockIds) $ \(orig, new) ->
    unless (new == "BL_" <> orig || new == orig) $ warn orig ("page id written as block id " <> new)
  let blocks = [(b, u) | (u@UBlock {}, b) <- zip units (expand units blockIds)]
      blockEls = numberEls 1 [els | UBlock _ _ _ els <- units]
  blocksJson <- forM (zip3 [0 :: Int ..] blocks blockEls) $ \(n, (bid, u), elsJson) -> case u of
    UBlock orig title _ _ -> do
      when (maybe False (not . M.null . i18nTranslations) title) $ warn bid "block title translations dropped"
      pure $
        object
          [ "Type" .= (if n == 0 then "Default" else "Standard" :: Text)
          , "SubType" .= ("" :: Text)
          , "Description" .= maybe orig i18nDefault title
          , "ID" .= bid
          , "BlockElements" .= elsJson
          , "Options" .= object ["BlockLocking" .= ("false" :: Text), "RandomizeQuestions" .= ("false" :: Text), "BlockVisibility" .= ("Expanded" :: Text)]
          ]
    UEmbedded {} -> pure Null
  let trash =
        object
          [ "Type" .= ("Trash" :: Text)
          , "Description" .= ("Trash / Unused Questions" :: Text)
          , "ID" .= ("BL_trash" :: Text)
          , "BlockElements" .= ([] :: [Value])
          , "Options" .= object ["BlockLocking" .= ("false" :: Text), "RandomizeQuestions" .= ("false" :: Text), "BlockVisibility" .= ("Collapsed" :: Text)]
          ]
      firstBlock = fmap fst (listToMaybe blocks)
  flowEls <- forM (zip [0 :: Int ..] (zip units (expand units blockIds))) $ \(n, (u, bid)) -> do
    let fid = "FL_" <> tshow (2 * n + 2)
    case u of
      UBlock _ _ rel _ ->
        branch env fid bid rel $
          object ["Type" .= (if Just bid == firstBlock then "Block" else "Standard" :: Text), "ID" .= bid, "FlowID" .= fid]
      UEmbedded rel qs ->
        branch env fid "embedded data" rel $
          object
            [ "Type" .= ("EmbeddedData" :: Text)
            , "FlowID" .= fid
            , "EmbeddedData" .= map embedded qs
            ]
  let flowCount = length units * 2 + 1
      title = maybe "" i18nDefault (metaTitle meta)
      el code prim sec pay =
        object
          [ "SurveyID" .= surveyId
          , "Element" .= (code :: Text)
          , "PrimaryAttribute" .= (prim :: Text)
          , "SecondaryAttribute" .= sec
          , "TertiaryAttribute" .= Null
          , "Payload" .= pay
          ]
      maxQid = maximum (0 : mapMaybe (\q -> T.stripPrefix "QID" q >>= readNat) qids)
      surveyTitle = case M.lookup "qualtrics:SurveyTitle" (metaAttrs meta) of
        Just (String t) -> t
        _ -> title
      option k = case M.lookup ("qualtrics:" <> k) (metaAttrs meta) of
        Just (String t) -> t
        _ -> fromMaybe "" (lookup k keptOptions)
      entry =
        object
          [ "SurveyID" .= surveyId
          , "SurveyName" .= title
          , "SurveyDescription" .= fmap i18nDefault (metaDescription meta)
          , "SurveyOwnerID" .= ("UR_panform" :: Text)
          , "SurveyBrandID" .= ("" :: Text)
          , "DivisionID" .= Null
          , "SurveyLanguage" .= T.toUpper dl
          , "SurveyActiveResponseSet" .= ("RS_panform" :: Text)
          , "SurveyStatus" .= ("Inactive" :: Text)
          , "SurveyStartDate" .= zeroDate
          , "SurveyExpirationDate" .= zeroDate
          , "SurveyCreationDate" .= zeroDate
          , "CreatorID" .= ("UR_panform" :: Text)
          , "LastModified" .= zeroDate
          , "LastAccessed" .= zeroDate
          , "LastActivated" .= zeroDate
          , "Deleted" .= Null
          ]
      options =
        object
          [ "BackButton" .= ("false" :: Text)
          , "SaveAndContinue" .= ("true" :: Text)
          , "SurveyProtection" .= option "SurveyProtection"
          , "BallotBoxStuffingPrevention" .= option "BallotBoxStuffingPrevention"
          , "NoIndex" .= ("Yes" :: Text)
          , "SecureResponseFiles" .= ("true" :: Text)
          , "SurveyExpiration" .= Null
          , "SurveyTermination" .= ("DefaultMessage" :: Text)
          , "Header" .= ("" :: Text)
          , "Footer" .= ("" :: Text)
          , "ProgressBarDisplay" .= ("None" :: Text)
          , "PartialData" .= ("+1 week" :: Text)
          , "ValidationMessage" .= Null
          , "PreviousButton" .= (" \8592 " :: Text)
          , "NextButton" .= (" \8594 " :: Text)
          , "SurveyTitle" .= surveyTitle
          , "SkinLibrary" .= ("Qualtrics" :: Text)
          , "SkinType" .= ("MQ" :: Text)
          , "Skin" .= ("qualtrics" :: Text)
          , "NewScoring" .= (1 :: Int)
          , "EOSMessage" .= ("" :: Text)
          , "ShowExportTags" .= ("false" :: Text)
          , "CollectGeoLocation" .= ("false" :: Text)
          , "SurveyMetaDescription" .= ("" :: Text)
          , "PasswordProtection" .= ("No" :: Text)
          , "AnonymizeResponse" .= option "AnonymizeResponse"
          , "Password" .= ("" :: Text)
          , "RefererCheck" .= option "RefererCheck"
          , "RefererURL" .= option "RefererURL"
          , "AvailableLanguages" .= object [K.fromText (T.toUpper l) .= ([] :: [Value]) | l <- dl : langs]
          ]
      scoring =
        object
          [ "ScoringCategories" .= ([] :: [Value])
          , "ScoringCategoryGroups" .= ([] :: [Value])
          , "ScoringSummaryCategory" .= Null
          , "ScoringSummaryAfterQuestions" .= (0 :: Int)
          , "ScoringSummaryAfterSurvey" .= (0 :: Int)
          , "DefaultScoringCategory" .= Null
          , "AutoScoringCategory" .= Null
          ]
      flowJson = object ["Type" .= ("Root" :: Text), "FlowID" .= ("FL_1" :: Text), "Flow" .= flowEls, "Properties" .= object ["Count" .= flowCount, "RemovedFieldsets" .= ([] :: [Value])]]
      elements =
        [ el "BL" "Survey Blocks" Null (toJSON (filter (/= Null) blocksJson ++ [trash]))
        , el "FL" "Survey Flow" Null flowJson
        ]
          ++ sqs
          ++ [ el "RS" "RS_panform" Null Null
             , el "SO" "Survey Options" Null options
             , el "SCO" "Scoring" Null scoring
             , object ["SurveyID" .= surveyId, "Element" .= ("PROJ" :: Text), "PrimaryAttribute" .= ("CORE" :: Text), "SecondaryAttribute" .= Null, "TertiaryAttribute" .= ("1.1.0" :: Text), "Payload" .= object ["ProjectCategory" .= ("CORE" :: Text), "SchemaVersion" .= ("1.1.0" :: Text)]]
             , el "STAT" "Survey Statistics" Null (object ["MobileCompatible" .= True, "ID" .= ("Survey Statistics" :: Text)])
             , el "QC" "Survey Question Count" (String (tshow maxQid)) Null
             ]
  pure (encodePretty (object ["SurveyEntry" .= entry, "SurveyElements" .= elements]))
  where
    zeroDate = "0000-00-00 00:00:00" :: Text
    isBreak = \case
      ElBreak -> True
      _ -> False
    -- block ids aligned with units (embedded units get a placeholder)
    expand us ids = go us ids
      where
        go (UBlock {} : rest) (i : is) = i : go rest is
        go (UEmbedded {} : rest) is = "" : go rest is
        go _ _ = []
    embedded q =
      object $
        [ "Description" .= qId q
        , "Type" .= (if isJust (qDefault q) then "Custom" else "Recipient" :: Text)
        , "Field" .= qId q
        , "VariableType" .= ("String" :: Text)
        , "DataVisibility" .= ([] :: [Value])
        , "AnalyzeText" .= False
        ]
          ++ ["Value" .= v | Just v <- [qDefault q]]

-- | Wrap a flow element in a branch when it has a condition.
branch :: M.Map Text WInfo -> Text -> Text -> Maybe Expr -> Value -> Conv Value
branch env fid at rel inner = do
  let branchId = maybe fid (\n -> "FL_" <> tshow (n + 1)) (T.stripPrefix "FL_" fid >>= readNat)
  logic <- writeLogic env at False rel
  pure $ case logic of
    Nothing -> inner
    Just l -> object ["Type" .= ("Branch" :: Text), "FlowID" .= branchId, "Description" .= ("New Branch" :: Text), "BranchLogic" .= l, "Flow" .= [inner]]

-- | Block ids Qualtrics accepts, unique, stable for ids already in form.
uniqueIds :: [Text] -> [Text]
uniqueIds = go S.empty
  where
    go _ [] = []
    go seen (i : rest) =
      let base = if "BL_" `T.isPrefixOf` i then i else "BL_" <> T.filter isAlphaNum i
          cands = base : [base <> tshow n | n <- [2 :: Int ..]]
          pick = head' (filter (`S.notMember` seen) cands)
       in pick : go (S.insert pick seen) rest
    head' (x : _) = x
    head' [] = "BL_"

elTag :: El -> Text
elTag = \case
  ElQ q -> qId q
  ElNote i _ _ -> i
  ElForm g -> groupId g
  ElBreak -> ""

elKind :: El -> Kind
elKind = \case
  ElQ q -> case qType q of
    QChoice s _ _ -> KChoice s
    QBoolean -> KChoice Single
    QScale {} | isNps q -> KNps
    QScale {} -> KSlider
    QText _ -> KText
    QNumber _ -> KText
    QDate _ -> KText
    _ -> KNone
  _ -> KNone

-- | Coded value to choice id.
elVals :: El -> M.Map Text Text
elVals e = M.fromList [(choiceValue c, i) | (i, c) <- zip (map tshow [1 :: Int ..]) (elChoices e)]

elChoices :: El -> [Choice]
elChoices = \case
  ElQ q -> case qType q of
    QChoice _ _ cs -> cs
    QBoolean -> boolChoices
    QScale lo hi _ _ | isNps q -> [choice (tshow n) (tshow n) | n <- [lo .. hi]]
    QRanking cs -> cs
    _ -> []
  _ -> []

-- | Choices and matrix rows and columns, whatever the type.
questionChoices :: QType -> [Choice]
questionChoices = \case
  QChoice _ _ cs -> cs
  QMatrix _ rs cs -> rs ++ cs
  QRanking cs -> cs
  _ -> []

-- | Question ids are numbered by position when written, so an id kept in
-- text or in raw content (piped text such as @${q://QID7/...}@, a kept
-- condition, a kept payload) can name a different question afterwards.
qidWarning :: Text
qidWarning = "refers to Qualtrics question ids (QID), which are renumbered when written; check what it points to"

warnQidRefs :: Text -> [I18n] -> Maybe Expr -> Conv ()
warnQidRefs at texts rel =
  when (any (T.isInfixOf "q://QID") (concatMap allText texts) || maybe False rawQid rel) $ warn at qidWarning
  where
    allText t = i18nDefault t : M.elems (i18nTranslations t)
    rawQid = \case
      ERaw _ src -> "QID" `T.isInfixOf` src
      ENot a -> rawQid a
      EBin _ a b -> rawQid a || rawQid b
      ECall _ as -> any rawQid as
      _ -> False

-- | A scale written as an NPS question: only 0 to 10 carries that selector
-- (any other range is written as a slider, whatever the attribute says).
isNps :: Question -> Bool
isNps q = case qType q of
  QScale 0 10 _ _ -> M.lookup "qualtrics:selector" (qAttrs q) == Just (String "NPS")
  _ -> False

boolChoices :: [Choice]
boolChoices = [choice "1" "Yes", choice "0" "No"]

elLangs :: El -> [Lang]
elLangs = \case
  ElQ q -> concatMap i18nLangs (qLabel q : map choiceLabel (qChoicesAll q))
  ElNote _ t _ -> i18nLangs t
  ElForm g -> concatMap i18nLangs (maybe [] pure (groupTitle g) ++ [qLabel q | ItemQuestion q <- groupItems g])
  ElBreak -> []
  where
    qChoicesAll q = case qType q of
      QChoice _ _ cs -> cs
      QMatrix _ rs cs -> rs ++ cs
      QRanking cs -> cs
      _ -> []

-- | Block elements, QIDs numbered by position across blocks (export tags
-- need not be unique).
numberEls :: Int -> [[El]] -> [[Value]]
numberEls _ [] = []
numberEls n (els : rest) = vs : numberEls n' rest
  where
    (n', vs) = foldl step (n, []) els
    step (k, acc) = \case
      ElBreak -> (k, acc ++ [object ["Type" .= ("Page Break" :: Text)]])
      _ -> (k + 1, acc ++ [object ["Type" .= ("Question" :: Text), "QuestionID" .= ("QID" <> tshow k)]])

-- | Rich text for a label: the original markup when it still says the same
-- thing, escaped plain text otherwise.
richText :: Attrs -> Text -> Text
richText attrs t = case M.lookup "qualtrics:html" attrs of
  Just (String h) | fst (stripHtml h) == t -> h
  _ -> escapeHtml t

writeEl :: M.Map Text WInfo -> [Lang] -> Text -> Text -> El -> Conv Value
writeEl env langs surveyId qid e = do
  (tag, payload) <- case e of
    ElNote i t rel -> do
      warnQidRefs i [t] rel
      logic <- writeLogic env i True rel
      pure (i, basePayload i (escapeHtml (i18nDefault t)) (i18nDefault t) "DB" "TB" Nothing [] [] (langObj t [] []) logic "OFF" [])
    ElForm g -> do
      warnQidRefs (groupId g) (maybe [] pure (groupTitle g) ++ [qLabel q | ItemQuestion q <- groupItems g]) (groupRelevant g)
      logic <- writeLogic env (groupId g) True (groupRelevant g)
      let fields = [c | ItemQuestion q <- groupItems g, let c = Choice "" (qLabel q) False False]
          title = fromMaybe (plain "") (groupTitle g)
          req = or [qRequired q | ItemQuestion q <- groupItems g]
          cs = zip (map tshow [1 :: Int ..]) fields
      pure (groupId g, basePayload (groupId g) (escapeHtml (i18nDefault title)) (i18nDefault title) "TE" "FORM" Nothing cs [] (langObj title cs []) logic (if req then "ON" else "OFF") [])
    ElQ q -> do
      logic <- writeLogic env (qId q) True (qRelevant q)
      (,) (qId q) <$> questionPayload q logic
    ElBreak -> pure ("", Null)
  let desc = case payload of
        Object o | Just (String d) <- KM.lookup "QuestionDescription" o -> d
        _ -> tag
  pure $
    object
      [ "SurveyID" .= surveyId
      , "Element" .= ("SQ" :: Text)
      , "PrimaryAttribute" .= qid
      , "SecondaryAttribute" .= desc
      , "TertiaryAttribute" .= Null
      , "Payload" .= payload
      ]
  where
    ids = map tshow [1 :: Int ..]
    -- A choice list as Qualtrics ids, with recodes when the codes are
    -- numeric and differ from the ids.
    basePayload :: Text -> Text -> Text -> Text -> Text -> Maybe Text -> [(Text, Choice)] -> [(Text, Choice)] -> Value -> Maybe Value -> Text -> [(K.Key, Value)] -> Value
    basePayload tag html plainText qt sel sub cs as lang logic force extra =
      let fields =
            [ "QuestionText" .= html
            , "DataExportTag" .= tag
            , "QuestionType" .= qt
            , "Selector" .= sel
            , "Configuration" .= object ["QuestionDescriptionOption" .= ("UseText" :: Text)]
            , "QuestionDescription" .= describe plainText
            , "Validation" .= object ["Settings" .= object ["ForceResponse" .= force, "ForceResponseType" .= ("ON" :: Text), "Type" .= ("None" :: Text)]]
            , "Language" .= lang
            , "NextChoiceId" .= (length cs + 1)
            , "NextAnswerId" .= (length as + 1)
            , "QuestionID" .= qid
            , "DataVisibility" .= object ["Private" .= False, "Hidden" .= False]
            , "ChoiceOrder" .= map fst cs
            ]
              ++ ["SubSelector" .= s | Just s <- [sub]]
              ++ ["DisplayLogic" .= l | Just l <- [logic]]
              ++ (if null cs then [] else ["Choices" .= object [K.fromText i .= choiceJson c | (i, c) <- cs]])
              ++ (if null as then [] else ["Answers" .= object [K.fromText i .= choiceJson c | (i, c) <- as], "AnswerOrder" .= map fst as])
       in object (overrideKeys extra fields)
    choiceJson c =
      object $
        ["Display" .= escapeHtml (i18nDefault (choiceLabel c))]
          ++ ["TextEntry" .= ("on" :: Text) | choiceOther c]
          ++ ["ExclusiveAnswer" .= True | choiceExclusive c]
    langObj :: I18n -> [(Text, Choice)] -> [(Text, Choice)] -> Value
    langObj t cs as =
      let per l =
            object $
              ["QuestionText" .= escapeHtml (i18nText (Just l) t)]
                ++ (if null cs then [] else ["Choices" .= object [K.fromText i .= object ["Display" .= escapeHtml (i18nText (Just l) (choiceLabel c))] | (i, c) <- cs]])
                ++ (if null as then [] else ["Answers" .= object [K.fromText i .= object ["Display" .= escapeHtml (i18nText (Just l) (choiceLabel c))] | (i, c) <- as]])
       in if null langs then toJSON ([] :: [Value]) else object [K.fromText (T.toUpper l) .= per l | l <- langs]
    describe t =
      let one = T.unwords (T.words t)
       in if T.length one > 100 then T.take 97 one <> "..." else one
    overrideKeys extra fields = extra ++ [f | f@(k, _) <- fields, k `notElem` map fst extra]
    numbered = zip ids
    -- Numeric codes become recode values; text codes, which Qualtrics
    -- recodes cannot hold, go in the choices' variable naming.
    codes :: [Choice] -> Conv ([(Text, Choice)], [(K.Key, Value)])
    codes cs = do
      let numeric = all (\c -> isJust (readNat (choiceValue c))) cs
          positional = and (zipWith (\i c -> i == choiceValue c) ids cs)
          field = if numeric then "RecodeValues" else "VariableNaming"
      pure
        ( numbered cs
        , [field .= object [K.fromText i .= choiceValue c | (i, c) <- numbered cs] | not positional]
        )
    questionPayload :: Question -> Maybe Value -> Conv Value
    questionPayload q logic = do
      let tag = qId q
          label = qLabel q
          html = richText (qAttrs q) (i18nDefault label)
          force = case M.lookup "qualtrics:ForceResponse" (qAttrs q) of
            Just (String f) | not (qRequired q) -> f
            _ -> if qRequired q then "ON" else "OFF"
          selAttr = case M.lookup "qualtrics:selector" (qAttrs q) of
            Just (String s) -> Just s
            _ -> Nothing
          mk qt sel sub cs as extra = basePayload tag html (i18nDefault label) qt sel sub cs as (langObj label cs as) logic force extra
          descriptive = basePayload tag html (i18nDefault label) "DB" "TB" Nothing [] [] (langObj label [] []) logic "OFF" []
      forM_ (qHint q) $ \_ -> warn tag "hint dropped"
      forM_ (qDefault q) $ \_ -> warn tag "default answer dropped"
      warnQidRefs tag ([label] ++ map choiceLabel (questionChoices (qType q))) (qRelevant q)
      -- number questions write their bounds; every other type has no
      -- place for a validation
      case qType q of
        QNumber _ -> pure ()
        QChoice {} -> pure ()
        QText _ -> pure ()
        QDate _ -> pure ()
        QMatrix {} -> pure ()
        _ -> checkValidation
      case qType q of
        QChoice sel style cs -> do
          (ids', recode) <- codes cs
          let s = case selAttr of
                Just a | a `elem` validSelectors sel -> a
                _ -> selectorFor sel style
          when (style == Buttons) $ warn tag "button style written as a plain choice list"
          checkValidation
          pure (mk "MC" s (if s `elem` ["DL", "SB", "MSB"] then Nothing else Just "TX") ids' [] recode)
        QBoolean -> do
          warn tag "yes/no written as a single choice"
          (ids', recode) <- codes boolChoices
          pure (mk "MC" "SAVR" (Just "TX") ids' [] recode)
        QText k -> do
          let (sel, ct) = case k of
                LongText -> ("ML", Nothing)
                Email -> ("SL", Just "ValidEmail")
                Phone -> ("SL", Just "ValidPhone")
                _ -> ("SL", Nothing)
              sel' = if selAttr `elem` [Just "PW", Just "ESTB"] then fromMaybe sel selAttr else sel
          when (k == Url) $ warn tag "url check dropped, written as a text box"
          checkValidation
          pure (mk "TE" sel' Nothing [] [] (textValidation force ct Nothing))
        QNumber k -> do
          when (k == Integer) $ warn tag "whole-number check written as a number check"
          forM_ (qValidation q >>= valExpr) $ \_ -> warn tag "validation expression dropped"
          forM_ (qValidation q >>= valMessage) $ \_ -> warn tag "validation message dropped"
          pure (mk "TE" "SL" Nothing [] [] (textValidation force (Just "ValidNumber") (qValidation q)))
        QDate k -> do
          unless (k == Date) $ warn tag "time written as a date check"
          checkValidation
          pure (mk "TE" "SL" Nothing [] [] (textValidation force (Just "ValidDate") Nothing))
        QMatrix sel rows cols -> do
          (colIds, recode) <- codes cols
          let rowIds = numbered rows
              positional = and (zipWith (\i c -> i == choiceValue c) ids rows)
              rowTags = if positional then Bool False else object [K.fromText i .= choiceValue c | (i, c) <- rowIds]
          checkValidation
          pure (mk "Matrix" "Likert" (Just (if sel == Multiple then "MultipleAnswer" else "SingleAnswer")) rowIds colIds (("ChoiceDataExportTags", rowTags) : recode))
        QScale lo hi l h
          | isNps q -> do
              when (isJust l || isJust h) $ warn tag "scale end labels dropped"
              (ids', recode) <- codes (elChoices (ElQ q))
              pure (mk "MC" "NPS" Nothing ids' [] recode)
          | otherwise -> do
              let labels = [lb | Just lb <- [l, h]]
              when (any (not . M.null . i18nTranslations) labels) $ warn tag "scale label translations dropped"
              when (isJust h && not (isJust l)) $ warn tag "high end label written as the low end label"
              pure $
                mk
                  "Slider"
                  (maybe "HSLIDER" id (selAttr >>= \s -> if s `elem` ["HBAR", "STAR"] then Just s else Nothing))
                  Nothing
                  [("1", choice "1" "")]
                  []
                  [ ( "Configuration"
                    , object
                        [ "QuestionDescriptionOption" .= ("UseText" :: Text)
                        , "CSSliderMin" .= lo
                        , "CSSliderMax" .= hi
                        , "GridLines" .= max 1 (hi - lo)
                        , "SnapToGrid" .= True
                        , "NumDecimals" .= ("0" :: Text)
                        , "ShowValue" .= True
                        , "CustomStart" .= False
                        , "NotApplicable" .= False
                        , "MobileFirst" .= True
                        ]
                    )
                  , ("Labels", object [K.fromText i .= object ["Display" .= escapeHtml (i18nDefault lb)] | (i, lb) <- numbered labels])
                  ]
        QRanking cs -> do
          (ids', recode) <- codes cs
          pure (mk "RO" (fromMaybe "DND" (selAttr >>= \s -> if s `elem` ["RB", "TB"] then Just s else Nothing)) (Just "TX") ids' [] recode)
        QFile -> pure (mk "FileUpload" "FileUpload" Nothing [] [] [])
        QGeo -> do
          warn tag "geopoint written as a text box"
          pure (mk "TE" "SL" Nothing [] [] [])
        QOther name -> case M.lookup "qualtrics:payload" (qAttrs q) of
          Just (Object o) -> do
            when (T.isInfixOf "QID" (TL.toStrict (TLE.decodeUtf8 (encode (Object (KM.delete "QuestionID" (KM.delete "DisplayLogic" o))))))) $
              warn tag qidWarning
            pure . Object $
              maybe id (KM.insert "DisplayLogic") logic $
                KM.insert "QuestionID" (String qid) $
                  KM.insert "DataExportTag" (String tag) $
                    KM.delete "DisplayLogic" o
          _ -> do
            warn tag ("unsupported type " <> name <> " written as descriptive text")
            when (qRequired q) $ warn tag "required flag dropped"
            pure descriptive
        QHidden -> pure descriptive
        QCalculate _ -> pure descriptive
      where
        checkValidation = forM_ (qValidation q) $ \_ -> warn (qId q) "validation dropped"
    validSelectors = \case
      Single -> ["SAVR", "SAHR", "SACOL", "DL", "SB"]
      Multiple -> ["MAVR", "MAHR", "MACOL", "MSB"]
    textValidation :: Text -> Maybe Text -> Maybe Validation -> [(K.Key, Value)]
    textValidation force ct v =
      [ ( "Validation"
        , object
            [ "Settings"
                .= object
                  ( [ "ForceResponse" .= (force :: Text)
                    , "ForceResponseType" .= ("ON" :: Text)
                    , "Type" .= (if isJust ct then "ContentType" else "None" :: Text)
                    ]
                      ++ ["ContentType" .= c | Just c <- [ct]]
                      ++ [ "ValidNumber" .= object ["Min" .= maybe "" numText (v >>= valMin), "Max" .= maybe "" numText (v >>= valMax)]
                         | ct == Just "ValidNumber"
                         ]
                  )
            ]
        )
      ]

embeddedCond :: Text -> Text -> Maybe Text -> Value
embeddedCond field op rhs =
  object $
    [ "LogicType" .= ("EmbeddedField" :: Text)
    , "LeftOperand" .= field
    , "Operator" .= op
    , "Type" .= ("Expression" :: Text)
    , "Description" .= ("" :: Text)
    ]
      ++ ["RightOperand" .= r | Just r <- [rhs]]

questionCond :: Text -> Text -> Text -> Maybe Text -> Value
questionCond qid loc op rhs =
  object $
    [ "LogicType" .= ("Question" :: Text)
    , "QuestionID" .= qid
    , "QuestionIsInLoop" .= ("no" :: Text)
    , "ChoiceLocator" .= loc
    , "Operator" .= op
    , "QuestionIDFromLocator" .= qid
    , "LeftOperand" .= loc
    , "Type" .= ("Expression" :: Text)
    , "Description" .= ("" :: Text)
    ]
      ++ ["RightOperand" .= r | Just r <- [rhs]]

-- | Logic as a single condition group: the expression in disjunctive normal
-- form, And binding tighter than Or. What Qualtrics cannot express is
-- dropped with a warning, like 'Panform.Expr.printLogic'.
writeLogic :: M.Map Text WInfo -> Text -> Bool -> Maybe Expr -> Conv (Maybe Value)
writeLogic _ _ _ Nothing = pure Nothing
writeLogic env at inPage (Just e) = case dnf (nnf e) >>= mapM (mapM (atomCond env)) of
  Right terms ->
    let conds = concat [zip (if n == 0 then "" : repeat "And" else "Or" : repeat "And") t | (n, t) <- zip [0 :: Int ..] terms]
        withConj (c, v) = case v of
          Object o | not (T.null c) -> Object (KM.insert "Conjuction" (String c) o)
          _ -> v
        grp = object ([K.fromText (tshow i) .= withConj c | (i, c) <- zip [0 :: Int ..] conds] ++ ["Type" .= ("If" :: Text)])
     in pure . Just . object $ ["0" .= grp, "Type" .= ("BooleanExpression" :: Text)] ++ ["inPage" .= False | inPage]
  Left why -> Nothing <$ warn at ("logic dropped, qualtrics cannot express " <> why)
  where
    nnf x = case x of
      ENot (EBin And a b) -> EBin Or (nnf (ENot a)) (nnf (ENot b))
      ENot (EBin Or a b) -> EBin And (nnf (ENot a)) (nnf (ENot b))
      ENot (ENot a) -> nnf a
      EBin And a b -> EBin And (nnf a) (nnf b)
      EBin Or a b -> EBin Or (nnf a) (nnf b)
      _ -> x
    dnf x = case x of
      EBin Or a b -> (++) <$> dnf a <*> dnf b
      EBin And a b -> do
        xs <- dnf a
        ys <- dnf b
        let r = [p ++ q | p <- xs, q <- ys]
        if length r > 64 then Left "a condition this large" else Right r
      _ -> Right [[x]]

atomCond :: M.Map Text WInfo -> Expr -> Either Text Value
atomCond env e = case e of
  ERaw "qualtrics" src -> case eitherDecode (BL.fromStrict (TE.encodeUtf8 src)) of
    Right v@(Object _) -> Right v
    _ -> Left ("condition " <> src)
  ELit (LBool True) -> Left "a constant condition"
  EAnswered q -> answered q True
  ENot (EAnswered q) -> answered q False
  ESelected q v -> selected q v "Selected"
  ENot (ESelected q v) -> selected q v "NotSelected"
  ENot x -> atomCond env x >>= negateCond
  EBin op (ERef q) (ELit l) -> compareC q op (litText l)
  EBin op (ELit l) (ERef q) -> compareC q (flipSides op) (litText l)
  _ -> Left "this expression"
  where
    look q = maybe (Left ("a reference to " <> q)) Right (M.lookup q env)
    answered q yes = do
      WInfo qid kind _ <- look q
      case kind of
        KChoice _ -> Right (questionCond qid ("q://" <> qid <> "/SelectedChoicesCount") (if yes then "GreaterThan" else "EqualTo") (Just "0"))
        KNps -> Right (questionCond qid ("q://" <> qid <> "/SelectedChoicesCount") (if yes then "GreaterThan" else "EqualTo") (Just "0"))
        KText -> Right (questionCond qid ("q://" <> qid <> "/ChoiceTextEntryValue") (emptyOp yes) Nothing)
        KSlider -> Right (questionCond qid ("q://" <> qid <> "/ChoiceNumericEntryValue/1") (emptyOp yes) Nothing)
        KHidden -> Right (embeddedCond q (emptyOp yes) Nothing)
        KNone -> Left ("an answered test on " <> q)
    emptyOp yes = if yes then "NotEmpty" else "Empty"
    selected q v op = do
      WInfo qid _ vals <- look q
      cid <- maybe (Left ("choice " <> v <> " of " <> q)) Right (M.lookup v vals)
      let loc = "q://" <> qid <> "/SelectableChoice/" <> cid
      Right (questionCond qid loc op Nothing)
    compareC q op rhs = do
      WInfo qid kind _ <- look q
      opName <- maybe (Left (T.pack (show op))) Right (lookup op [(o, n) | (n, o) <- opNames])
      case kind of
        KChoice _ | op `elem` [Eq, Neq] -> selected q rhs (if op == Eq then "Selected" else "NotSelected")
        KNps | op `elem` [Eq, Neq] -> selected q rhs (if op == Eq then "Selected" else "NotSelected")
        KText -> Right (questionCond qid ("q://" <> qid <> "/ChoiceTextEntryValue") opName (Just rhs))
        KSlider -> Right (questionCond qid ("q://" <> qid <> "/ChoiceNumericEntryValue/1") opName (Just rhs))
        KHidden -> Right (embeddedCond q opName (Just rhs))
        _ -> Left ("a " <> T.pack (show op) <> " comparison on " <> q)
    flipSides op = case op of
      Lt -> Gt
      Gt -> Lt
      Le -> Ge
      Ge -> Le
      o -> o
    litText = \case
      LStr s -> s
      LNum n -> numText n
      LBool b -> if b then "1" else "0"

negateCond :: Value -> Either Text Value
negateCond = \case
  Object o | Just (String op) <- KM.lookup "Operator" o, Just op' <- lookup op flips -> Right (Object (KM.insert "Operator" (String op') o))
  _ -> Left "this negation"
  where
    flips =
      concat
        [ [(a, b), (b, a)]
        | (a, b) <-
            [ ("Selected", "NotSelected")
            , ("EqualTo", "NotEqualTo")
            , ("GreaterThan", "LessThanOrEqual")
            , ("LessThan", "GreaterThanOrEqual")
            , ("Empty", "NotEmpty")
            ]
        ]
