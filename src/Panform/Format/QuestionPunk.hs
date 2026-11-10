-- | QuestionPunk Markdown (@.qp.md@): a questionnaire written the way it
-- looks on paper. It is valid Markdown, lossless against the panform AST,
-- and short enough for a person or a language model to write by hand.
--
-- > ---
-- > title: Customer feedback
-- > languages: en, fr
-- > ---
-- >
-- > # About you {#p1}
-- >
-- > 1. Have you used our product? {#used *}
-- >    @fr Avez-vous utilisé notre produit ?
-- >    ( ) Yes {#yes}
-- >    ( ) No {#no}
-- >
-- > 2. Which features do you use? {#features}
-- >    if used = yes
-- >    [ ] Reports
-- >    [ ] Other {#other other}
-- >    [ ] None of these {#none only}
-- >
-- > 3. How likely are you to recommend us? {#nps}
-- >    [0] Not likely .. [10] Very likely
--
-- The rules:
--
-- * Items are separated by blank lines. A @#@ heading opens a page, @##@ a
--   section, and a @repeat@ attribute makes either a loop.
-- * A section runs to the next heading of its level. A line of heading
--   marks alone (@#@, @##@) ends it sooner, so the items after it sit
--   outside the section.
-- * A question starts with a number (@1.@); the number is cosmetic.
-- * The line's trailing @{...}@ holds attributes: @#id@, @*@ (required), a
--   type word (@long@, @email@, @url@, @phone@, @integer@, @number@, @date@,
--   @time@, @datetime@, @yesno@, @file@, @location@, @hidden@, @calc@,
--   @dropdown@, @buttons@; @choice@ or @multiple@ for a choice question whose
--   options are not listed), @min=@/@max=@ and @key=value@ extensions.
-- * Indented lines describe the answer: @( )@ pick one, @[ ]@ pick many,
--   @-@ rows (with options: a matrix; alone: a ranking), @[lo] a .. [hi] b@
--   a scale, @> @ a hint, @if@ a display condition, @check@ and @!@ a
--   validation rule and its message, @=@ a calculation, @default@ a default.
-- * @\@fr text@ translates the element on the line above.
-- * Anything else is a note. A fenced block tagged with a format name is
--   raw content for that format.
module Panform.Format.QuestionPunk
  ( readQPMarkdown
  , writeQPMarkdown
  ) where

import Control.Monad (foldM, when)
import Data.Aeson (Value (..), eitherDecodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Expr.QP
import Panform.Format.QuestionPunkJson (detailKeys)

------------------------------------------------------------------------------
-- Writing

writeQPMarkdown :: Form -> Conv BL.ByteString
writeQPMarkdown (Form meta items) = do
  (_, body) <- blocks 1 1 items
  let out = T.intercalate "\n\n" (filter (not . T.null) (frontMatter meta : body)) <> "\n"
  pure (BL.fromStrict (TE.encodeUtf8 out))

-- | Blocks at a heading level; question numbers run on across groups.
blocks :: Int -> Int -> [Item] -> Conv (Int, [Text])
blocks lvl n0 = fmap (\(n, acc, _) -> (n, acc)) . foldM step (n0, [], False)
  where
    -- A heading owns everything up to the next heading, so an item that
    -- follows a section at the same level needs the section ended first: a
    -- line of this level's heading marks alone.
    step (n, acc, afterGroup) i = do
      (n', out) <- item n i
      let closing = [T.replicate (min 6 lvl) "#" | afterGroup, not (isGroup i)]
      pure (n', acc ++ closing ++ out, isGroup i)
    isGroup = \case ItemGroup _ -> True; _ -> False
    item n = \case
      ItemGroup g -> do
        when (lvl > 6) $ warn (groupId g) "sections nested deeper than six levels are written at level six, so this one and the items after it read back under another section"
        rel <- logicLine (groupId g) (groupRelevant g)
        let natural = if lvl == 1 then Page else Section
            kindAttr = case groupKind g of
              Repeat -> ["repeat"]
              k | k /= natural -> [if k == Page then "page" else "section"]
              _ -> []
            heading =
              T.replicate (min 6 lvl) "#" <> maybe "" ((" " <>) . inl . i18nDefault) (groupTitle g)
                <> attrBlock (quoteVal ("#" <> groupId g) : kindAttr ++ attrTokens (groupAttrs g))
            detail = maybe [] translations (groupTitle g) ++ rel
        (n', kids) <- blocks (lvl + 1) n (groupItems g)
        pure (n', T.intercalate "\n" (T.stripEnd heading : map indent detail) : kids)
      ItemQuestion q -> do
        b <- questionBlock n q
        pure (n + 1, [b])
      ItemNote i t rel -> do
        r <- logicLine i rel
        let body = i18nDefault t
            ls = T.splitOn "\n" body
            attrs = if T.null i then "" else attrBlock [quoteVal ("#" <> i)]
            textLines
              | all safeInline ls = case ls of
                  (l : more) -> (escapeNote l <> attrs) : map escapeNote more
                  [] -> [attrs]
              | otherwise = [jsonStr body <> attrs]
        pure (n, [T.intercalate "\n" (textLines ++ map indent (translations t ++ r))])
      ItemRaw (Format f) v ->
        pure (n, ["```" <> f <> "\n" <> TE.decodeUtf8 (BL.toStrict (encode v)) <> "\n```"])

frontMatter :: Meta -> Text
frontMatter m
  | null ls = ""
  | otherwise = T.intercalate "\n" (["---"] ++ ls ++ ["---"])
  where
    ls =
      [ "id: " <> i | Just i <- [metaId m]]
        ++ maybe [] (field "title") (metaTitle m)
        ++ maybe [] (field "description") (metaDescription m)
        ++ ["languages: " <> T.intercalate ", " (metaLanguages m) | not (null (metaLanguages m))]
        ++ [k <> ": " <> jsonOrText v | (k, v) <- M.toList (metaAttrs m)]
    field k (I18n d tr) = (k <> ": " <> inl d) : [k <> "@" <> l <> ": " <> inl t | (l, t) <- M.toList tr]

-- | A value as written: plain text when it reads back as the same string,
-- JSON otherwise.
jsonOrText :: Value -> Text
jsonOrText = \case
  String s | not (T.null s), s == T.strip s, not (T.any (== '\n') s), parseValue s == String s -> s
  v -> TE.decodeUtf8 (BL.toStrict (encode v))

indent :: Text -> Text
indent = ("   " <>)

-- | Text in a one-line slot (a hint, choice, translation, title): as-is when
-- it reads back unchanged, otherwise a JSON string literal.
inl :: Text -> Text
inl t = if safeInline t then t else jsonStr t

safeInline :: Text -> Bool
safeInline t =
  not (T.null t)
    && T.strip t == t
    && not (T.any (== '\n') t)
    && not ("\"" `T.isPrefixOf` t)
    && not ("}" `T.isSuffixOf` t)
    && not (" .. [" `T.isInfixOf` t)

jsonStr :: Text -> Text
jsonStr = TE.decodeUtf8 . BL.toStrict . encode . String

-- | The inverse of 'inl'.
uninl :: Text -> Text
uninl t = case T.uncons t of
  Just ('"', _) | Right (String v) <- eitherDecodeStrict (TE.encodeUtf8 t) -> v
  _ -> t

-- | A label line may not look like a marker or a keyword line.
escapeLine :: Text -> Text
escapeLine t
  | any (`T.isPrefixOf` t) ["( )", "[ ]", "- ", "> ", "@", "if ", "check ", "! ", "= ", "default ", "min ", "max ", "\\", "["] = "\\" <> t
  | otherwise = t

escapeNote :: Text -> Text
escapeNote t
  | T.null t = "\\"
  | T.take 1 t `elem` ["#", "-", "`", "\\", " "] || isNumbered t = "\\" <> t
  | otherwise = t

isNumbered :: Text -> Bool
isNumbered t = let (d, rest) = T.span isDigit t in not (T.null d) && ". " `T.isPrefixOf` (rest <> " ")

attrBlock :: [Text] -> Text
attrBlock [] = ""
attrBlock ts = " {" <> T.unwords ts <> "}"

attrTokens :: Attrs -> [Text]
attrTokens a = [k <> "=" <> quoteVal (jsonOrText v) | (k, v) <- M.toList a]

quoteVal :: Text -> Text
quoteVal v
  | not (T.null v), T.all (\c -> not (isSpace c) && c `notElem` ("{}\"=" :: String)) v = v
  | otherwise = jsonStr v

translations :: I18n -> [Text]
translations (I18n _ tr) = ["@" <> l <> " " <> inl t | (l, t) <- M.toList tr]

logicLine :: Text -> Maybe Expr -> Conv [Text]
logicLine _ Nothing = pure []
logicLine at (Just e) = case printQP e of
  Right t -> pure ["if " <> t]
  Left why -> [] <$ warn at ("logic dropped, cannot express " <> why)

questionBlock :: Int -> Question -> Conv Text
questionBlock n q = do
  rel <- logicLine (qId q) (qRelevant q)
  (typeWords, body) <- answerLines q
  val <- validationLines q
  mapM_ (\k -> warn (qId q) ("attribute " <> k <> " reads back as questionpunk:" <> k)) [k | k <- M.keys (qAttrs q), k `elem` detailKeys, k `notElem` reservedAttrs]
  let labelText = i18nDefault (qLabel q)
      labelLines = T.splitOn "\n" labelText
      (first, rest)
        | T.null labelText = ("", [])
        | all safeInline labelLines, (l : ls) <- labelLines = (escapeLine l, map escapeLine ls)
        | otherwise = (jsonStr labelText, [])
      attrs = quoteVal ("#" <> qId q) : ["*" | qRequired q] ++ typeWords ++ attrTokens (M.mapKeys bareKey (qAttrs q))
      header = T.pack (show n) <> "." <> (if T.null first then "" else " " <> first) <> attrBlock attrs
      detail =
        rest
          ++ translations (qLabel q)
          ++ maybe [] (\h -> ("> " <> inl (i18nDefault h)) : translations h) (qHint q)
          ++ rel
          ++ ["default " <> inl d | Just d <- [qDefault q]]
          ++ body
          ++ val
  pure (T.intercalate "\n" (header : map indent detail))

validationLines :: Question -> Conv [Text]
validationLines q = case qValidation q of
  Nothing -> pure []
  Just v -> do
    ex <- case valExpr v of
      Nothing -> pure []
      Just e -> case printQP e of
        Right t -> pure ["check " <> t]
        Left why -> [] <$ warn (qId q) ("validation dropped, cannot express " <> why)
    pure $
      ["min " <> num x | Just x <- [valMin v]]
        ++ ["max " <> num x | Just x <- [valMax v]]
        ++ ex
        ++ maybe [] (\m -> ("! " <> inl (i18nDefault m)) : translations m) (valMessage v)

num :: Double -> Text
num x = if x == fromIntegral (round x :: Integer) then T.pack (show (round x :: Integer)) else T.pack (show x)

-- | Type words for the attribute block, and the answer lines.
answerLines :: Question -> Conv ([Text], [Text])
answerLines q = case qType q of
  QText k -> pure ([w | Just w <- [lookup k [(LongText, "long"), (Email, "email"), (Url, "url"), (Phone, "phone")]]], [])
  QNumber Integer -> pure (["integer"], [])
  QNumber Decimal -> pure (["number"], [])
  QDate k -> pure ([case k of Date -> "date"; Time -> "time"; DateTime -> "datetime"], [])
  QBoolean -> pure (["yesno"], [])
  QFile -> pure (["file"], [])
  QGeo -> pure (["location"], [])
  QHidden -> pure (["hidden"], [])
  QCalculate (ELit (LStr "")) -> pure (["calc"], [])
  QCalculate e -> case printQP e of
    Right t -> pure (["calc"], ["= " <> t])
    Left why -> (["calc"], []) <$ warn (qId q) ("calculation dropped, cannot express " <> why)
  QOther t -> pure (["type=" <> quoteVal t], [])
  QChoice sel style cs ->
    pure
      ( -- With no options listed, the kind of choice is named instead.
        [if sel == Multiple then "multiple" else "choice" | null cs]
          ++ [w | Just w <- [lookup style [(Dropdown, "dropdown"), (Buttons, "buttons")]]]
      , concatMap (choiceLine (marker sel)) cs
      )
  QMatrix sel rows cols -> do
    when (null rows || null cols) $ warn (qId q) "matrix without rows or without answers reads back as another question type"
    pure ([], concatMap (choiceLine "-") rows ++ concatMap (choiceLine (marker sel)) cols)
  QRanking cs -> do
    when (null cs) $ warn (qId q) "ranking without items reads back as a short text question"
    pure ([], concatMap (choiceLine "-") cs)
  QScale lo hi l h ->
    let side v lbl = "[" <> T.pack (show v) <> "]" <> maybe "" ((" " <>) . inl . i18nDefault) lbl
        trs = M.keys (M.union (maybe M.empty i18nTranslations l) (maybe M.empty i18nTranslations h))
        tr lang = "@" <> lang <> " " <> maybe "" (inl . i18nText (Just lang)) l <> " .. " <> maybe "" (inl . i18nText (Just lang)) h
     in pure ([], (side lo l <> " .. " <> side hi h) : map tr trs)
  where
    marker Single = "( )"
    marker Multiple = "[ ]"

choiceLine :: Text -> Choice -> [Text]
choiceLine m c =
  let lbl = i18nDefault (choiceLabel c)
      attrs = [quoteVal ("#" <> choiceValue c) | choiceValue c /= lbl] ++ ["other" | choiceOther c] ++ ["only" | choiceExclusive c]
   in (m <> " " <> inl lbl <> attrBlock attrs) : translations (choiceLabel c)

------------------------------------------------------------------------------
-- Reading

readQPMarkdown :: BL.ByteString -> Conv Form
readQPMarkdown bs = do
  let text = TE.decodeUtf8Lenient (BL.toStrict bs)
      -- Editors on Windows save a byte-order mark, which would hide the front matter.
      ls = T.lines (T.replace "\r\n" "\n" (fromMaybe text (T.stripPrefix "\xFEFF" text)))
      (fm, rest) = splitFrontMatter ls
  meta <- readFrontMatter fm
  items <- concat <$> mapM readBlock (paragraphs rest)
  let stray = strayCloses items
  when (stray > 0) $
    warn "sections" (tshowInt stray <> " line(s) of heading marks alone end no open section and were ignored")
  pure (Form meta (nest items))

splitFrontMatter :: [Text] -> ([Text], [Text])
splitFrontMatter ls = case dropWhile (T.null . T.strip) ls of
  ("---" : more) -> let (fm, rest) = break (== "---") more in (fm, drop 1 rest)
  other -> ([], other)

readFrontMatter :: [Text] -> Conv Meta
readFrontMatter = foldM step emptyMeta
  where
    -- Split on ": " so a key may itself contain a colon (@questionpunk:x: 1@).
    step m l = case T.breakOn ": " (if ":" `T.isSuffixOf` l then l <> " " else l) of
      (_, "") -> pure m
      (k0, v0) -> do
        let k = T.strip k0
            raw = T.strip (T.drop 1 v0)
            v = uninl raw
            (base, lang) = case T.breakOn "@" k of
              (b, l') | not (T.null l') -> (b, Just (T.drop 1 l'))
              _ -> (k, Nothing)
            setI18n old = case lang of
              Nothing -> Just (maybe (plain v) (\o -> o {i18nDefault = v}) old)
              Just lg -> Just ((fromMaybe (plain "") old) {i18nTranslations = M.insert lg v (maybe M.empty i18nTranslations old)})
        pure $ case base of
          "id" -> m {metaId = Just v}
          "title" -> m {metaTitle = setI18n (metaTitle m)}
          "description" -> m {metaDescription = setI18n (metaDescription m)}
          "languages" -> m {metaLanguages = filter (not . T.null) (map T.strip (T.splitOn "," v))}
          _ -> m {metaAttrs = M.insert k (parseValue raw) (metaAttrs m)}

-- | A value written by 'jsonOrText'.
parseValue :: Text -> Value
parseValue v
  | any (`T.isPrefixOf` v) ["\"", "{", "[", "true", "false", "null"] || isNumber v =
      either (const (String v)) id (eitherDecodeStrict (TE.encodeUtf8 v))
  | otherwise = String v
  where
    isNumber t = not (T.null t) && T.all (\c -> isDigit c || c `elem` ("-.eE+" :: String)) t && T.any isDigit t

-- | Blank-line separated blocks; fenced code keeps its blank lines.
paragraphs :: [Text] -> [[Text]]
paragraphs = go []
  where
    go acc [] = flush acc []
    go acc (l : ls)
      | "```" `T.isPrefixOf` l =
          let (body, rest) = break ("```" `T.isPrefixOf`) ls
           in flush acc ((l : body ++ ["```"]) : go [] (drop 1 rest))
      | T.null (T.strip l) = flush acc (go [] ls)
      | otherwise = go (l : acc) ls
    flush [] r = r
    flush acc r = reverse acc : r

-- | A flat item, with its heading level when it opens a group; 'Close' ends
-- the sections of that level and deeper.
data Flat = Heading Int Group | Close Int | Leaf Item

readBlock :: [Text] -> Conv [Flat]
readBlock [] = pure []
readBlock ls@(first : rest)
  | "```" `T.isPrefixOf` first = do
      let fmt = T.strip (T.drop 3 first)
          body = T.unlines (takeWhile (not . ("```" `T.isPrefixOf`)) rest)
      pure [Leaf (ItemRaw (Format fmt) (either (const (String body)) id (eitherDecodeStrict (TE.encodeUtf8 body))))]
  | T.all (== '#') (T.strip first), null rest = pure [Close (T.length (T.strip first))]
  | "#" `T.isPrefixOf` first = do
      let (hashes, title) = T.span (== '#') first
          (text, attrs) = splitAttrs (T.strip title)
          ats = attrWords attrs
          lvl = T.length hashes
          kind
            | "repeat" `elem` ats = Repeat
            | "page" `elem` ats = Page
            | "section" `elem` ats = Section
            | lvl == 1 = Page
            | otherwise = Section
          detail = map T.strip rest
      rel <- relevance "group" detail
      let titleI = if T.null text then Nothing else Just (applyTranslations (plain (uninl (unescape text))) [t | t <- detail, "@" `T.isPrefixOf` t])
      pure [Heading lvl (Group (idOf ats) kind titleI rel [] (extraAttrs ats))]
  | isNumbered first = pure . Leaf . ItemQuestion <$> readQuestion ls
  | otherwise = do
      let (textLines, detail) = break (\l -> "   " `T.isPrefixOf` l) ls
          (firstText, attrs) = splitAttrs (headOr "" textLines)
          body
            | "\"" `T.isPrefixOf` firstText = uninl firstText
            | otherwise = T.intercalate "\n" (map unescape (firstText : drop 1 textLines))
          d = map T.strip detail
      rel <- relevance "note" d
      pure [Leaf (ItemNote (idOf (attrWords attrs)) (applyTranslations (plain body) [t | t <- d, "@" `T.isPrefixOf` t]) rel)]

headOr :: a -> [a] -> a
headOr d = \case
  (x : _) -> x
  [] -> d

unescape :: Text -> Text
unescape t = fromMaybe t (T.stripPrefix "\\" t)

relevance :: Text -> [Text] -> Conv (Maybe Expr)
relevance at ls = case [T.drop 3 l | l <- ls, "if " `T.isPrefixOf` l] of
  (src : _) -> Just <$> logic at src
  [] -> pure Nothing

logic :: Text -> Text -> Conv Expr
logic at src = case parseQP src of
  Right e -> pure e
  Left err -> ERaw "questionpunk" src <$ warn at ("could not parse logic \"" <> src <> "\": " <> T.unwords (T.words (T.pack (unlines (drop 3 (lines err))))))

-- | Apply @\@lang text@ lines to an 'I18n'.
applyTranslations :: I18n -> [Text] -> I18n
applyTranslations = foldl' add
  where
    add i l = case T.breakOn " " (T.drop 1 l) of
      (lang, t) | not (T.null lang) -> i {i18nTranslations = M.insert lang (uninl (T.drop 1 t)) (i18nTranslations i)}
      _ -> i

-- | "Label {#id * long}" -> ("Label", "#id * long"). The attribute block is
-- the first " {" whose contents, read with JSON-quoted values, close exactly
-- at the end of the line; braces elsewhere in a label survive.
splitAttrs :: Text -> (Text, Text)
splitAttrs t0 = go 0
  where
    t = T.stripEnd t0
    go from = case T.breakOn "{" (T.drop from t) of
      (_, "") -> (t0, "")
      (pre, rest) ->
        let at = from + T.length pre
            before = T.take at t
            inner = T.drop 1 rest
         in if (T.null before || " " `T.isSuffixOf` before) && closesAtEnd inner
              then (T.stripEnd before, T.dropEnd 1 inner)
              else go (at + 1)
    -- The block's text ends at the first unquoted "}", which must be last.
    closesAtEnd x = case scan x of
      Just n -> n == T.length x - 1
      Nothing -> False
    scan x = loop 0 False (T.unpack x)
      where
        loop _ _ [] = Nothing
        loop i True ('\\' : _ : cs) = loop (i + 2) True cs
        loop i q ('"' : cs) = loop (i + 1) (not q) cs
        loop i False ('}' : _) = Just i
        loop _ False ('{' : _) = Nothing
        loop i q (_ : cs) = loop (i + 1) q cs

-- | Attribute tokens. A quoted value is a JSON string literal:
-- @label="two words"@ gives @label=two words@.
attrWords :: Text -> [Text]
attrWords = go . T.stripStart
  where
    go t
      | T.null t = []
      | otherwise =
          let (key, rest) = T.break (\c -> isSpace c || c == '"') t
           in case T.uncons rest of
                Just ('"', _) ->
                  let (lit, r') = T.splitAt (literalLength rest) rest
                      tok = if T.null key then uninl lit else T.dropWhileEnd (== '=') key <> "=" <> uninl lit
                   in tok : go (T.stripStart r')
                _ -> key : go (T.stripStart rest)
    -- Length of the JSON string literal at the start, quotes included.
    literalLength x = walk 1 (T.unpack (T.drop 1 x))
      where
        walk n ('\\' : _ : cs) = walk (n + 2) cs
        walk n ('"' : _) = n + 1
        walk n (_ : cs) = walk (n + 1) cs
        walk n [] = n

idOf :: [Text] -> Text
idOf ws = headOr "" [T.drop 1 w | w <- ws, "#" `T.isPrefixOf` w]

-- | @key=value@ tokens (and @key"value@ from quoted ones) as attributes.
extraAttrs :: [Text] -> Attrs
extraAttrs ws =
  M.fromList
    [ (k, parseValue v)
    | w <- ws
    , Just (k, v) <- [kv w]
    , k `notElem` reservedAttrs
    ]
  where
    kv w = case T.break (== '=') w of
      (k, r) | not (T.null k), Just (_, v) <- T.uncons r -> Just (k, v)
      _ -> Nothing

-- | Attribute keys the syntax itself uses.
reservedAttrs :: [Text]
reservedAttrs = ["min", "max", "type"]

-- | A QuestionPunk item-detail key is written bare (@questionMode=ai_decides@)
-- unless the syntax already uses that name (@min=@, @max=@, @type=@).
bareKey :: Text -> Text
bareKey k = case T.stripPrefix "questionpunk:" k of
  Just b | b `elem` detailKeys, b `notElem` reservedAttrs -> b
  _ -> k

-- | The reverse of 'bareKey' for a question's attributes.
questionAttrs :: [Text] -> Attrs
questionAttrs = M.mapKeys (\k -> if k `elem` detailKeys && k `notElem` reservedAttrs then "questionpunk:" <> k else k) . extraAttrs

attrValue :: Text -> [Text] -> Maybe Text
attrValue k ws = headOr Nothing [Just (T.drop (T.length k + 1) w) | w <- ws, (k <> "=") `T.isPrefixOf` w]

-- | A detail line of a question.
data Line
  = LOption Selection Choice
  | LRow Choice
  | LScale Int (Maybe Text) Int (Maybe Text)
  | LHint Text
  | LIf Text
  | LCheck Text
  | LMessage Text
  | LMin Double
  | LMax Double
  | LCalc Text
  | LDefault Text
  | LTranslate Text
  | LText Text

classify :: Text -> Line
classify l0 = case l0 of
  _ | Just r <- T.stripPrefix "( ) " l0 <|> exact "( )" -> LOption Single (optionOf r)
  _ | Just r <- T.stripPrefix "[ ] " l0 <|> exact "[ ]" -> LOption Multiple (optionOf r)
  _ | Just r <- T.stripPrefix "- " l0 -> LRow (optionOf r)
  _ | Just s <- scaleOf l0 -> s
  _ | Just r <- T.stripPrefix "> " l0 -> LHint (uninl r)
  _ | Just r <- T.stripPrefix "if " l0 -> LIf r
  _ | Just r <- T.stripPrefix "check " l0 -> LCheck r
  _ | Just r <- T.stripPrefix "! " l0 -> LMessage (uninl r)
  _ | Just r <- T.stripPrefix "min " l0, Just d <- readD r -> LMin d
  _ | Just r <- T.stripPrefix "max " l0, Just d <- readD r -> LMax d
  _ | Just r <- T.stripPrefix "= " l0 -> LCalc r
  _ | Just r <- T.stripPrefix "default " l0 -> LDefault (uninl r)
  _ | "@" `T.isPrefixOf` l0 -> LTranslate l0
  _ -> LText (unescape l0)
  where
    exact m = if l0 == m then Just "" else Nothing
    a <|> b = maybe b Just a
    readD t = case reads (T.unpack t) of
      [(d, "")] -> Just d
      _ -> Nothing
    optionOf r =
      let (lbl, attrs) = splitAttrs r
          ws = attrWords attrs
          lbl' = uninl lbl
          v = case [T.drop 1 w | w <- ws, "#" `T.isPrefixOf` w] of
            (x : _) -> x
            [] -> lbl'
       in Choice v (plain lbl') ("other" `elem` ws) ("only" `elem` ws)
    scaleOf l = do
      r1 <- T.stripPrefix "[" l
      let (lo, r2) = T.span (\c -> isDigit c || c == '-') r1
      r3 <- T.stripPrefix "]" r2
      let (loLbl, r4) = T.breakOn " .. [" r3
      r5 <- T.stripPrefix " .. [" r4
      let (hi, r6) = T.span (\c -> isDigit c || c == '-') r5
      r7 <- T.stripPrefix "]" r6
      loN <- readI lo
      hiN <- readI hi
      pure (LScale loN (nonEmpty (T.strip loLbl)) hiN (nonEmpty (T.strip r7)))
    readI t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing
    nonEmpty t = if T.null t then Nothing else Just t

readQuestion :: [Text] -> Conv Question
readQuestion [] = failWith "empty question"
readQuestion (first : rest) = do
  let afterNum = T.stripStart (T.drop 1 (T.dropWhile isDigit first))
      (label0, attrs) = splitAttrs afterNum
      ws = attrWords attrs
      qid = idOf ws
      detail = map (classify . T.strip) rest
      -- Translations attach to the element on the line above.
      attach :: [Line] -> ([Text], [(Line, [Text])])
      attach = foldr step ([], [])
        where
          step (LTranslate t) (pending, acc) = (t : pending, acc)
          step l (pending, acc) = ([], (l, pending) : acc)
      (leading, lines') = attach detail
      labelTrs = leading ++ concat [trs | (LText _, trs) <- lines']
      extraLabel = [t | (LText t, _) <- lines']
      labelBase
        | "\"" `T.isPrefixOf` label0 = uninl label0
        | otherwise = T.intercalate "\n" (unescape label0 : extraLabel)
      label = applyTranslations (plain labelBase) labelTrs
      opts = [(s, c {choiceLabel = applyTranslations (choiceLabel c) trs}) | (LOption s c, trs) <- lines']
      rows = [c {choiceLabel = applyTranslations (choiceLabel c) trs} | (LRow c, trs) <- lines']
      hint = case [(h, trs) | (LHint h, trs) <- lines'] of
        ((h, trs) : _) -> Just (applyTranslations (plain h) trs)
        [] -> Nothing
      sel = case opts of
        ((s, _) : _) -> s
        [] -> Single
      style
        | "dropdown" `elem` ws = Dropdown
        | "buttons" `elem` ws = Buttons
        | otherwise = Radio
      has w = w `elem` ws
  when (T.null qid) $ warn (T.take 30 label0) "question without an {#id}; using an empty id"
  rel <- case [e | (LIf e, _) <- lines'] of
    (e : _) -> Just <$> logic qid e
    [] -> pure Nothing
  chk <- case [e | (LCheck e, _) <- lines'] of
    (e : _) -> Just <$> logic qid e
    [] -> pure Nothing
  calc <- case [e | (LCalc e, _) <- lines'] of
    (e : _) -> Just <$> logic qid e
    [] -> pure Nothing
  let msg = case [(m, trs) | (LMessage m, trs) <- lines'] of
        ((m, trs) : _) -> Just (applyTranslations (plain m) trs)
        [] -> Nothing
      mn = headOr Nothing [Just d | (LMin d, _) <- lines']
      mx = headOr Nothing [Just d | (LMax d, _) <- lines']
      validation =
        if isJust mn || isJust mx || isJust chk || isJust msg
          then Just (Validation mn mx chk msg)
          else Nothing
      scale = headOr Nothing [Just (lo, l, hi, h, trs) | (LScale lo l hi h, trs) <- lines']
      qt
        | Just (lo, l, hi, h, trs) <- scale =
            let side f = fmap (\x -> applyTranslations (plain (uninl x)) [t | t0 <- trs, Just t <- [splitScaleTr f t0]])
             in QScale lo hi (side fst l) (side snd h)
        | not (null rows), not (null opts) = QMatrix sel rows (map snd opts)
        | not (null rows) = QRanking rows
        | not (null opts) = QChoice sel style (map snd opts)
        | has "multiple" = QChoice Multiple style []
        | has "choice" || has "dropdown" || has "buttons" = QChoice Single style []
        | has "long" = QText LongText
        | has "email" = QText Email
        | has "url" = QText Url
        | has "phone" = QText Phone
        | has "integer" = QNumber Integer
        | has "number" = QNumber Decimal
        | has "date" = QDate Date
        | has "time" = QDate Time
        | has "datetime" = QDate DateTime
        | has "yesno" = QBoolean
        | has "file" = QFile
        | has "location" = QGeo
        | has "hidden" = QHidden
        | has "calc" = QCalculate (fromMaybe (ELit (LStr "")) calc)
        | Just t <- attrValue "type" ws = QOther t
        | otherwise = QText ShortText
  pure
    Question
      { qId = qid
      , qLabel = label
      , qHint = hint
      , qType = qt
      , qRequired = has "*"
      , qRelevant = rel
      , qValidation = validation
      , qDefault = headOr Nothing [Just d | (LDefault d, _) <- lines']
      , qAttrs = questionAttrs ws
      }
  where
    -- "@fr Peu .. Très" -> the low or high half, as a translation line.
    splitScaleTr f t = case T.breakOn " " (T.drop 1 t) of
      (lang, body) ->
        let (lo, hi) = T.breakOn " .. " (T.drop 1 body)
            part = T.strip (f (lo, T.drop 4 hi))
         in if T.null part then Nothing else Just ("@" <> lang <> " " <> part)

-- | Lines of heading marks alone that end no open section.
strayCloses :: [Flat] -> Int
strayCloses = go [] 0
  where
    go _ n [] = n
    go open n (Heading l _ : rest) = go (l : dropWhile (>= l) open) n rest
    go open n (Close l : rest) = case dropWhile (> l) open of
      o : more | o == l -> go more n rest
      _ -> go (dropWhile (>= l) open) (n + 1) rest
    go open n (Leaf _ : rest) = go open n rest

tshowInt :: Int -> Text
tshowInt = T.pack . show

-- | Rebuild the tree from headings: a heading owns everything up to the next
-- heading of the same or a shallower level, or a line of heading marks alone.
nest :: [Flat] -> [Item]
nest = fst . go 0
  where
    go :: Int -> [Flat] -> ([Item], [Flat])
    go _ [] = ([], [])
    go lvl fs@(Heading l g : rest)
      | l <= lvl = ([], fs)
      | otherwise =
          let (kids, rest') = go l rest
              (sibs, rest'') = go lvl rest'
           in (ItemGroup g {groupItems = kids} : sibs, rest'')
    go lvl fs@(Close l : rest)
      | l <= lvl = ([], fs)
      | otherwise = go lvl rest
    go lvl (Leaf i : rest) = let (sibs, r) = go lvl rest in (i : sibs, r)

