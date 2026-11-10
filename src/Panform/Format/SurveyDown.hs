-- | surveydown (surveydown.org): a Quarto document whose pages are
-- @::: {.sd_page id=...}@ divs and whose questions are R calls,
-- @sd_question(id, type, label, option = c("Label" = "value"))@ (labels
-- name the values). Display logic and required questions live in the R
-- server function in @app.R@; the writer leaves them in a
-- @<!-- panform:app.R ... -->@ comment block, which the reader reads back.
module Panform.Format.SurveyDown (readSurveyDown, writeSurveyDown) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Void (Void)
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Format.TextUtil
import Text.Megaparsec hiding (choice, chunk)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------------------------
-- A little R: calls with named arguments, strings, numbers and c() vectors

type Parser = Parsec Void Text

data RVal = RStr Text | RNum Double | RBool Bool | RNull | RVec [(Maybe Text, RVal)] | RExpr Text
  deriving (Eq, Show)

data RCall = RCall Text [(Maybe Text, RVal)] Text
  -- ^ Function, arguments, source text.

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sym :: Text -> Parser Text
sym = L.symbol sc

rIdent :: Parser Text
rIdent = lexeme (T.pack <$> ((:) <$> satisfy (\c -> isAlphaNum c || c == '.') <*> many (satisfy (\c -> isAlphaNum c || c `elem` ("._" :: String)))))

rString :: Parser Text
rString = lexeme (T.pack <$> (str '"' <|> str '\''))
  where
    str q = char q *> manyTill L.charLiteral (char q)

rValue :: Parser RVal
rValue =
  P.choice
    [ RStr <$> rString
    , try (RNum <$> lexeme (L.signed (pure ()) (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))))
    , RBool True <$ try (sym "TRUE" <* notFollowedBy (satisfy isAlphaNum))
    , RBool False <$ try (sym "FALSE" <* notFollowedBy (satisfy isAlphaNum))
    , RNull <$ try (sym "NULL" <* notFollowedBy (satisfy isAlphaNum))
    , try (sym "c" *> (RVec <$> between (sym "(") (sym ")") (rArg `sepEndBy` sym ",")))
    , RExpr . fst <$> match (rIdent *> optional (between (sym "(") (sym ")") (rArg `sepEndBy` sym ",")))
    ]

rArg :: Parser (Maybe Text, RVal)
rArg = (,) <$> optional (try ((rString <|> rIdent) <* sym "=" <* notFollowedBy (char '='))) <*> rValue

rCall :: Parser RCall
rCall = do
  (src, (f, args)) <- match ((,) <$> rIdent <*> between (sym "(") (sym ")") (rArg `sepEndBy` sym ","))
  pure (RCall f args (T.strip src))

rString' :: Text -> Text
rString' t = "\"" <> T.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      c -> T.singleton c

------------------------------------------------------------------------------
-- Reading

data Block
  = BPage Text
  | BEnd
  | BChunk Text
  | BHeading Text
  | BPara [Text]
  | BLogic [Text]

readSurveyDown :: BL.ByteString -> Conv Form
readSurveyDown bs = do
  let ls = map (T.dropWhileEnd isSpace) (T.lines (T.replace "\r" "" (TE.decodeUtf8With TE.lenientDecode (BL.toStrict bs))))
      (yaml, body) = frontMatter ls
      meta =
        emptyMeta
          { metaTitle = plain <$> lookup "title" yaml
          , metaDescription = plain <$> lookup "description" yaml
          , metaLanguages = maybe [] pure (lookup "lang" yaml)
          }
      bl = blocks body
  items <- build bl
  let logicLines = concat [l | BLogic l <- bl]
      conds = M.fromListWith (\new old -> EBin Or old new) (mapMaybe condition logicLines)
      required = concatMap requiredIds logicLines
  items' <- applyLogic conds required items
  pure (Form meta items')

frontMatter :: [Text] -> ([(Text, Text)], [Text])
frontMatter ls = case ls of
  ("---" : rest) ->
    let (y, more) = break (== "---") rest
     in ([(T.strip k, yamlValue (T.strip (T.drop 1 v))) | l <- y, let (k, v) = T.breakOn ":" l, not (T.null v)], drop 1 more)
  _ -> ([], ls)
  where
    yamlValue v
      | "\"" `T.isPrefixOf` v, Right t <- eitherDecode (BL.fromStrict (TE.encodeUtf8 v)) = t
      | Just x <- T.stripPrefix "'" v >>= T.stripSuffix "'" = x
      | otherwise = v

blocks :: [Text] -> [Block]
blocks = \case
  [] -> []
  l : rest
    | T.all isSpace l -> blocks rest
    | Just p <- pageStart l -> BPage p : blocks rest
    | T.all (== ':') l, T.length l >= 3 -> BEnd : blocks rest
    | "```{r" `T.isPrefixOf` l ->
        let (code, more) = break ("```" `T.isPrefixOf`) rest in BChunk (T.unlines code) : blocks (drop 1 more)
    | "<!-- panform:app.R" `T.isPrefixOf` l ->
        let (lg, more) = break ("-->" `T.isPrefixOf`) rest in BLogic lg : blocks (drop 1 more)
    | "#" `T.isPrefixOf` l -> BHeading (T.strip (T.dropWhile (== '#') l)) : blocks rest
    | otherwise ->
        let (p, more) = break (\x -> T.all isSpace x || isJust (pageStart x) || "```" `T.isPrefixOf` x || "<!--" `T.isPrefixOf` x || "#" `T.isPrefixOf` x) rest
         in BPara (l : p) : blocks more
  where
    pageStart l
      | Just r <- T.stripPrefix "::: {" l, ".sd_page" `T.isInfixOf` r =
          Just (T.takeWhile (\c -> c /= '}' && not (isSpace c)) (T.drop 3 (snd (T.breakOn "id=" r))))
      | Just r <- T.stripPrefix "--- " l, not (T.null (T.strip r)), T.all (\c -> isAlphaNum c || c `elem` ("_-" :: String)) (T.strip r) = Just (T.strip r)
      | otherwise = Nothing

-- | Items, with pages as groups. Page titles are a page's first heading;
-- an italic line right after a question is its hint.
build :: [Block] -> Conv [Item]
build = go Nothing [] [] (1 :: Int)
  where
    -- current page (id, title, items reversed), finished items reversed
    go cur pageAcc done n = \case
      [] -> pure (reverse (close cur pageAcc done))
      BPage p : rest -> go (Just (p, Nothing)) [] (close cur pageAcc done) n rest
      BEnd : rest -> go Nothing [] (close cur pageAcc done) n rest
      BLogic _ : rest -> go cur pageAcc done n rest
      BHeading h : rest -> case cur of
        Just (p, Nothing) | null pageAcc -> go (Just (p, Just h)) pageAcc done n rest
        _ -> add cur (ItemNote (noteId n) (plain h) Nothing) pageAcc done (n + 1) rest
      BPara [l] : rest
        | Just h <- T.stripPrefix "*" l >>= T.stripSuffix "*"
        , ItemQuestion q : prev <- target cur pageAcc done
        , Nothing <- qHint q ->
            replaceLast cur (ItemQuestion q {qHint = Just (plain h)} : prev) pageAcc done n rest
      BPara ps : rest -> add cur (ItemNote (noteId n) (plain (T.intercalate "\n" (map unescLine ps))) Nothing) pageAcc done (n + 1) rest
      BChunk code : rest -> do
        is <- chunk code
        let (pa, d) = foldl (\(pa0, d0) i -> push cur i pa0 d0) (pageAcc, done) is
        go cur pa d n rest
    noteId n = "note" <> T.pack (show n)
    unescLine l = fromMaybe l (T.stripPrefix "\\" l)
    target cur pageAcc done = maybe done (const pageAcc) cur
    push cur i pageAcc done = case cur of
      Just _ -> (i : pageAcc, done)
      Nothing -> (pageAcc, i : done)
    add cur i pageAcc done n rest = let (pa, d) = push cur i pageAcc done in go cur pa d n rest
    replaceLast cur is pageAcc done n rest = case cur of
      Just _ -> go cur is done n rest
      Nothing -> go cur pageAcc is n rest
    close cur pageAcc done = case cur of
      Just (p, t) -> ItemGroup (Group p Page (plain <$> t) Nothing (reverse pageAcc) M.empty) : done
      Nothing -> done

-- | The items of an R chunk: questions from @sd_question@, other calls
-- kept raw.
chunk :: Text -> Conv [Item]
chunk code = case parse (sc *> many rCall <* eof) "chunk" code of
  Left _
    | T.all isSpace code -> pure []
    | otherwise -> [ItemRaw (Format "surveydown") (String (T.strip code))] <$ warn "surveydown" "R chunk not understood; kept raw"
  Right calls -> fmap concat . forM calls $ \(RCall f args src) -> case f of
    "sd_question" -> pure <$> sdQuestion args
    "library" -> pure []
    "sd_nav" -> pure []
    "sd_next" -> pure []
    _ -> [ItemRaw (Format "surveydown") (String src)] <$ warn "surveydown" ("call to " <> f <> " kept raw")

sdQuestion :: [(Maybe Text, RVal)] -> Conv Item
sdQuestion args = do
  let named = [(k, v) | (Just k, v) <- args]
      positional = [v | (Nothing, v) <- args]
      str k = case lookup k named of
        Just (RStr s) -> Just s
        _ -> Nothing
      -- sd_question(id, type, label, ...) when not named
      pos i k = case (str k, drop i positional) of
        (Just s, _) -> Just s
        (Nothing, RStr s : _) -> Just s
        _ -> Nothing
      qid = fromMaybe "q" (pos 0 "id")
      ty = pos 1 "type"
      label = fromMaybe qid (pos 2 "label")
      vec k = case lookup k named of
        Just (RVec xs) -> [(n, s) | (n, RStr s) <- xs] ++ [(n, T.pack (show d)) | (n, RNum d) <- xs]
        _ -> []
      opts = [Choice v (plain (fromMaybe v l)) False False | (l, v) <- vec "option"]
      rows = [Choice v (plain (fromMaybe v l)) False False | (l, v) <- vec "row"]
      known = ["id", "type", "label", "option", "row", "default", "selected"]
      extra = [("surveydown:" <> k, json v) | (k, v) <- named, k `notElem` known, Just _ <- [simple v]]
  mapM_ (\(k, _) -> warn qid ("argument " <> k <> " dropped")) [(k, v) | (k, v) <- named, k `notElem` known, Nothing <- [simple v]]
  qt <- case ty of
    Just "mc" -> pure (QChoice Single Radio opts)
    Just "select" -> pure (QChoice Single Dropdown opts)
    Just "mc_buttons" -> pure (QChoice Single Buttons opts)
    Just "mc_multiple" -> pure (QChoice Multiple Radio opts)
    Just "mc_multiple_buttons" -> pure (QChoice Multiple Buttons opts)
    Just "mc_image" -> QChoice Single Buttons opts <$ warn qid "image options read as plain options"
    Just "mc_multiple_image" -> QChoice Multiple Buttons opts <$ warn qid "image options read as plain options"
    Just "text" -> pure (QText ShortText)
    Just "textarea" -> pure (QText LongText)
    Just "numeric" -> pure (QNumber Decimal)
    Just "date" -> pure (QDate Date)
    Just "daterange" -> QDate Date <$ warn qid "date range read as a single date"
    Just "slider" -> QChoice Single Buttons opts <$ warn qid "slider read as button choices"
    Just "slider_numeric" -> QNumber Decimal <$ warn qid "numeric slider read as a number"
    Just "matrix" -> pure (QMatrix Single rows opts)
    Just "matrix_multiple" -> pure (QMatrix Multiple rows opts)
    Just other -> QOther other <$ warn qid ("unknown question type " <> other <> " kept by name")
    Nothing -> QOther "unknown" <$ warn qid "question without a type (defined in questions.yml?)"
  pure . ItemQuestion $
    (question qid label qt)
      { qDefault = str "default" `orElse` str "selected"
      , qAttrs = M.fromList extra
      }
  where
    orElse a b = maybe b Just a
    simple = \case
      RStr s -> Just (String s)
      RNum d -> Just (Number (fromFloatDigits d))
      RBool b -> Just (Bool b)
      _ -> Nothing
    json v = fromMaybe Null (simple v)

-- | @cond ~ "target",@ from an @sd_show_if@ call.
condition :: Text -> Maybe (Text, Expr)
condition l = case T.breakOnEnd " ~ " (T.strip l) of
  (pre, post)
    | not (T.null pre) ->
        let tgt = T.dropWhileEnd (== ',') (T.strip post)
         in case T.stripPrefix "\"" tgt >>= T.stripSuffix "\"" of
              Just t -> Just (t, parseOrRaw rlang (T.strip (T.dropEnd 3 pre)))
              Nothing -> Nothing
  _ -> Nothing

requiredIds :: Text -> [Text]
requiredIds l = case T.breakOn "required_questions" l of
  (_, rest) | not (T.null rest) -> case parse (sym "required_questions" *> sym "=" *> rValue) "required" rest of
    Right (RVec xs) -> [s | (_, RStr s) <- xs]
    Right (RStr s) -> [s]
    _ -> []
  _ -> []

applyLogic :: M.Map Text Expr -> [Text] -> [Item] -> Conv [Item]
applyLogic conds required items = do
  let ids = map itemId (allItems items)
  mapM_ (\t -> warn t "sd_show_if target not found") [t | t <- M.keys conds, t `notElem` ids]
  pure (map go items)
  where
    go = \case
      ItemGroup g -> ItemGroup g {groupRelevant = M.lookup (groupId g) conds, groupItems = map go (groupItems g)}
      ItemQuestion q -> ItemQuestion q {qRelevant = M.lookup (qId q) conds, qRequired = qRequired q || qId q `elem` required}
      i -> i

------------------------------------------------------------------------------
-- Writing

writeSurveyDown :: Form -> Conv BL.ByteString
writeSurveyDown s@(Form meta items) = do
  warnTranslations "surveydown" s
  pages <- forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    flat <- flattenGroups kids
    body <- concat <$> mapM writeItem flat
    p <- pid n g
    -- A page title is one heading line.
    title <- case g >>= groupTitle of
      Just t | T.unwords (T.words (txt t)) /= txt t -> Just (T.unwords (T.words (txt t))) <$ warn p "page title line breaks and repeated or surrounding spaces written as single spaces"
      t -> pure (txt <$> t)
    pure (["::: {.sd_page id=" <> p <> "}"] ++ ["## " <> t | Just t <- [title]] ++ body ++ [":::"])
  allConds <- fmap concat . forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    pc <- cond (pageTarget n g) (g >>= groupRelevant)
    qcs <- fmap concat . forM (allItems kids) $ \case
      ItemQuestion q | written q -> cond (qId q) (qRelevant q)
      ItemNote i _ (Just _) -> [] <$ warn i "note display logic dropped"
      _ -> pure []
    pure (pc ++ qcs)
  let requiredAll = [qId q | q <- questions s, qRequired q, written q]
      required = filter (`notElem` shared) requiredAll
  mapM_ (\i -> warn i "required flag dropped; the id is not unique") (filter (`elem` shared) requiredAll)
  let yaml =
        ["---"]
          ++ ["title: " <> yamlStr (txt t) | Just t <- [metaTitle meta]]
          ++ ["description: " <> yamlStr (txt d) | Just d <- [metaDescription meta]]
          ++ ["lang: " <> l | (l : _) <- [metaLanguages meta]]
          ++ ["format: html", "echo: false", "warning: false", "---"]
      server =
        [ T.unlines $
            ["<!-- panform:app.R display logic and required questions; paste into the server function in app.R", "", "server <- function(input, output, session) {"]
              ++ (if null allConds then [] else ["  sd_show_if("] ++ zipWith (\i c -> "    " <> c <> (if i < length allConds then "," else "")) [1 :: Int ..] allConds ++ ["  )"])
              ++ ["  sd_server(db = NULL" <> (if null required then "" else ", required_questions = c(" <> T.intercalate ", " (map rString' required) <> ")") <> ")", "}", "-->"]
        | not (null allConds && null required)
        ]
  unless (null allConds) $ warn "surveydown" "display logic written as an app.R comment block; paste it into the server function"
  unless (null required) $ warn "surveydown" "required questions written in the app.R comment block"
  pure . BL.fromStrict . TE.encodeUtf8 . T.intercalate "\n\n" $
    [T.intercalate "\n" yaml, "```{r}\nlibrary(surveydown)\n```"]
      ++ [T.intercalate "\n\n" ls | ls <- pages]
      ++ map T.stripEnd server
      ++ [""]
  where
    qids = map qId (questions s)
    -- sd_show_if targets pages and questions by id, so a page may not share
    -- a question's id.
    -- Page ids go unquoted in a div attribute, so they are kept to word
    -- characters (idempotently).
    pageTarget n g =
      let p0 = pageId n g
          p1 = if T.null p0 then "page" <> T.pack (show n) else T.map (\c -> if isAlphaNum c || c `elem` ("_-" :: String) then c else '_') p0
       in if p1 `elem` qids then p1 <> "_page" else p1
    pid n g = do
      let p = pageTarget n g
      p <$ when (p /= pageId n g) (warn (pageId n g) ("page id written as " <> p))
    -- A condition on an id two items share would read back onto both.
    targets = [pageTarget n g | (n, (g, _)) <- zip [1 :: Int ..] (paginate items)] ++ [qId q | q <- questions s, written q]
    shared = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(t, 1) | t <- targets]))
    written q = case qType q of
      QHidden -> False
      QCalculate _ -> False
      _ -> True
    cond t = \case
      Nothing -> pure []
      Just _ | t `elem` shared -> [] <$ warn t "display logic dropped; the id is not unique"
      Just e -> do
        r <- printLogic rlang t (Just (numeric e))
        pure [c <> " ~ " <> rString' t | Just c <- [r]]
    yamlStr t = TE.decodeUtf8With TE.lenientDecode (BL.toStrict (encode (String t)))

-- | R compares strings lexically, so numeric comparisons coerce the answer.
numeric :: Expr -> Expr
numeric = \case
  EBin op (ERef q) r@(ELit (LNum _)) | op `elem` [Lt, Le, Gt, Ge] -> EBin op (ECall "as.numeric" [ERef q]) r
  EBin op a b -> EBin op (numeric a) (numeric b)
  ENot x -> ENot (numeric x)
  e -> e

writeItem :: Item -> Conv [Text]
writeItem = \case
  ItemNote i t _
    | T.all isSpace (txt t) -> [] <$ warn i "empty note dropped"
    | otherwise -> do
        -- Lines are trimmed: the reader drops trailing space, and leading
        -- space would turn a line into a code block.
        let lines' = map (escLine . T.strip) (T.lines (T.strip (txt t)))
            -- A line the reader would take for structure is escaped, as
            -- Markdown allows.
            escLine l
              | any (`T.isPrefixOf` l) ["#", ":::", "```", "<!--", "--- ", "\\", "*"] = "\\" <> l
              | otherwise = l
        when (T.intercalate "\n" (map T.strip (T.lines (T.strip (txt t)))) /= T.strip (txt t)) $ warn i "note line indentation and trailing spaces trimmed"
        [T.intercalate "\n" lines'] <$ warn i "note id dropped; notes are plain text"
  ItemRaw (Format "surveydown") (String t) -> pure ["```{r}\n" <> t <> "\n```"]
  ItemRaw (Format f) _ -> [] <$ warn "surveydown" ("raw " <> f <> " content dropped")
  ItemGroup g -> flattenGroups [ItemGroup g] >>= fmap concat . mapM writeItem
  ItemQuestion q -> writeQuestion q

writeQuestion :: Question -> Conv [Text]
writeQuestion q = do
  let at = qId q
  mapM_ (const (warn at "validation dropped")) (qValidation q)
  -- A hint is one line of Markdown.
  when (maybe False (\h -> T.unwords (T.words (txt h)) /= txt h) (qHint q)) $ warn at "hint line breaks and repeated or surrounding spaces written as single spaces"
  typed <- case qType q of
    QText ShortText -> pure (Just ("text", []))
    QText LongText -> pure (Just ("textarea", []))
    QText k -> Just ("text", []) <$ warn at (T.toLower (T.pack (show k)) <> " written as text")
    QNumber Decimal -> pure (Just ("numeric", []))
    QNumber Integer -> Just ("numeric", []) <$ warn at "whole-number restriction dropped"
    QDate Date -> pure (Just ("date", []))
    QDate k -> Just ("text", []) <$ warn at (T.toLower (T.pack (show k)) <> " written as text")
    QChoice sel style cs -> do
      flags cs
      ty <- case (sel, style) of
        (Single, Radio) -> pure "mc"
        (Single, Dropdown) -> pure "select"
        (Single, Buttons) -> pure "mc_buttons"
        (Multiple, Buttons) -> pure "mc_multiple_buttons"
        (Multiple, Radio) -> pure "mc_multiple"
        (Multiple, Dropdown) -> "mc_multiple" <$ warn at "multi-select dropdown written as checkboxes"
      pure (Just (ty, [("option", vector cs)]))
    QMatrix sel rows cols -> do
      flags (rows ++ cols)
      pure (Just (if sel == Single then "matrix" else "matrix_multiple", [("row", vector rows), ("option", vector cols)]))
    QScale lo hi l h -> Just ("mc_buttons", [("option", vector (scaleChoices lo hi l h))]) <$ warn at "scale written as buttons"
    QRanking cs -> Just ("mc_multiple", [("option", vector cs)]) <$ warn at "ranking written as a multiple choice"
    QBoolean -> Just ("mc", [("option", vector [choice "true" "Yes", choice "false" "No"])]) <$ warn at "yes/no written as a choice"
    QFile -> Just ("text", []) <$ warn at "file upload written as text"
    QGeo -> Just ("text", []) <$ warn at "geopoint written as text"
    QOther t -> Just ("text", []) <$ warn at ("type " <> t <> " written as text")
    QHidden -> Nothing <$ warn at "hidden field dropped; set it with sd_store_value() in app.R"
    QCalculate _ -> Nothing <$ warn at "calculated field dropped; compute it with sd_store_value() in app.R"
  pure $ case typed of
    Nothing -> []
    Just (ty, extra) ->
      let args =
            [("id", rString' at), ("type", rString' ty), ("label", rString' (txt (qLabel q)))]
              ++ extra
              ++ [("default", rString' d) | Just d <- [qDefault q]]
              ++ [(k, v) | (ak, val) <- M.toList (qAttrs q), Just k <- [T.stripPrefix "surveydown:" ak], Just v <- [rLit val]]
       in ["```{r}\nsd_question(\n" <> T.intercalate ",\n" ["  " <> k <> " = " <> v | (k, v) <- args] <> "\n)\n```"]
            ++ ["*" <> h <> "*" | Just h <- [T.unwords . T.words . txt <$> qHint q], not (T.null h)]
  where
    flags cs = do
      when (any choiceOther cs) $ warn (qId q) "other free-text box dropped"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive option flag dropped"
    vector cs = "c(\n" <> T.intercalate ",\n" ["    " <> rString' (txt (choiceLabel c)) <> " = " <> rString' (choiceValue c) | c <- cs] <> "\n  )"
    rLit = \case
      String t -> Just (rString' t)
      Number n -> Just (showNumber (toRealFloat n))
      Bool b -> Just (if b then "TRUE" else "FALSE")
      _ -> Nothing
