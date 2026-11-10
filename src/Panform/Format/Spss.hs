{-# LANGUAGE MultiWayIf #-}

-- | SPSS command syntax (@.sps@) as a data dictionary: an @INPUT PROGRAM@
-- declaring each variable's format, then @VARIABLE LABELS@,
-- @VALUE LABELS@, @VARIABLE LEVEL@, @MISSING VALUES@ and, for multi-select
-- questions, @MRSETS /MDGROUP@ over one 0/1 variable per option. SPSS has
-- no pages, question logic or survey texts beyond the labels.
module Panform.Format.Spss (readSpss, writeSpss) where

import Control.Monad (when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Void (Void)
import Panform.AST hiding (choice)
import Panform.Conv
import Panform.Format.Codebook
import Text.Megaparsec
import Text.Megaparsec.Char

------------------------------------------------------------------------------
-- Lexing

data Tok = TWord Text | TStr Text | TNum Text | TSym Char
  deriving (Eq, Show)

type Parser = Parsec Void Text

tokens' :: Parser [Tok]
tokens' = space *> many (tok <* space) <* eof
  where
    tok =
      choice
        [ TStr <$> strLit
        , TNum <$> try num
        , TWord <$> word
        , TSym <$> satisfy (`elem` ("/()=,+-<>*&|~" :: String))
        ]
    strLit = do
      s <- quoted
      rest <- many (try (space *> char '+' *> space *> quoted))
      pure (T.concat (s : rest))
    quoted = q '\'' <|> q '"'
    q c = char c *> (T.concat <$> many (T.singleton c <$ try (string (T.pack [c, c])) <|> T.singleton <$> satisfy (/= c))) <* char c
    num = do
      sign <- option "" (T.singleton <$> char '-')
      ds <- takeWhile1P Nothing (\c -> isDigit c || c == '.')
      notFollowedBy (satisfy isWordChar)
      pure (sign <> ds)
    word = do
      c <- satisfy (\x -> isAlpha x || x `elem` ("$#@_" :: String))
      rest <- takeWhileP Nothing isWordChar
      pure (T.dropWhileEnd (== '.') (T.cons c rest))
    isWordChar x = isAlphaNum x || x `elem` ("$#@_." :: String)

-- | Split a file into commands. A command ends with a period that is the
-- last non-blank character on its line, outside quotes. Which quote is
-- open (@'@ or @"@) is followed from character to character and from line
-- to line, so the other quote character inside a string, or a doubled
-- quote, does not unbalance it. Comments (@* ...@, @COMMENT ...@) and
-- @/* ... */@ outside a string are dropped.
commands :: Text -> [Text]
commands = go Nothing [] . T.lines
  where
    go _ acc [] = [T.unwords (reverse acc) | not (all (T.null . T.strip) acc)]
    go open acc (l0 : ls)
      | null acc && T.null (T.strip l) = go Nothing [] ls
      | null acc && isComment l = go Nothing [] (dropComment (l0 : ls))
      | open' == Nothing && endsPlain l = T.unwords (reverse (T.dropEnd 1 (T.stripEnd l) : acc)) : go Nothing [] ls
      | otherwise = go open' (l : acc) ls
      where
        (l, open') = scanLine open l0
    isComment l = let s = T.stripStart l in "*" `T.isPrefixOf` s || T.toUpper (T.take 7 s) == "COMMENT"
    -- Quotes mean nothing inside a comment.
    dropComment ls = drop 1 (dropWhile (not . endsPlain . fst . scanLine Nothing) ls)
    endsPlain l = "." `T.isSuffixOf` T.stripEnd l
    -- One line without its @/* ... */@ comments, and the quote left open
    -- at its end.
    scanLine :: Maybe Char -> Text -> (Text, Maybe Char)
    scanLine open l = let (out, open') = walk open (T.unpack l) in (T.pack out, open')
    walk open = \case
      [] -> ([], open)
      '/' : '*' : r | open == Nothing -> walk Nothing (afterComment r)
      c : r
        | open == Nothing && (c == '\'' || c == '"') -> keep c (walk (Just c) r)
        | open == Just c -> keep c (walk Nothing r)
        | otherwise -> keep c (walk open r)
    afterComment = \case
      '*' : '/' : r -> r
      _ : r -> afterComment r
      [] -> []
    keep c ~(out, open) = (c : out, open)

------------------------------------------------------------------------------
-- Reading

data VarInfo = VarInfo
  { viFormat :: Maybe Text
  , viLabel :: Maybe Text
  , viValues :: [(Text, Text)]
  , viLevel :: Maybe Text
  , viMissing :: Maybe Text
  }

data MrSet = MrSet
  { mrName :: Text
  , mrLabel :: Text
  , mrVars :: [Text]
  , mrValue :: Text
  , mrCatLabels :: Text
  }

data Dict = Dict
  { dRev :: [Text]
  -- ^ Variable names, newest first.
  , dUpper :: M.Map Text Text
  -- ^ Each name by its upper-case spelling: SPSS names are
  -- case-insensitive, and looking one up must not scan every variable.
  , dVars :: M.Map Text VarInfo
  , dSets :: [MrSet]
  , dTitle :: Maybe Text
  }

-- | Variable names in the order they were first seen.
dOrder :: Dict -> [Text]
dOrder = reverse . dRev

emptyInfo :: VarInfo
emptyInfo = VarInfo Nothing Nothing [] Nothing Nothing

readSpss :: BL.ByteString -> Conv Form
readSpss bs = do
  let src = TE.decodeUtf8With TE.lenientDecode (BL.toStrict bs)
  dict0 <- foldlM' command (Dict [] M.empty M.empty [] Nothing) (commands src)
  -- variable names are case-insensitive: a set member is the variable
  let upper = M.fromList [(T.toUpper n, n) | n <- reverse (dOrder dict0)]
      dict = dict0 {dSets = [st {mrVars = [M.findWithDefault v (T.toUpper v) upper | v <- mrVars st]} | st <- dSets dict0]}
      setVars = concatMap mrVars (dSets dict)
      byName n = M.findWithDefault emptyInfo n (dVars dict)
      firstIx = M.fromList (zip (dOrder dict) [0 :: Int ..])
      setItems = [(minimum (maxBound : mapMaybe (`M.lookup` firstIx) (mrVars s)), setQuestion byName s) | s <- dSets dict]
  varItems <- sequence [(\q -> (i, q)) <$> varQuestion n (byName n) | (i, n) <- zip [0 ..] (dOrder dict), n `notElem` setVars]
  let qs = map snd (sortOn fst (setItems ++ varItems))
  pure (Form emptyMeta {metaTitle = plain <$> dTitle dict} (map ItemQuestion qs))
  where
    foldlM' f z xs = foldl' (\m x -> m >>= \a -> f a x) (pure z) xs

-- | Apply one command to the dictionary.
command :: Dict -> Text -> Conv Dict
command d src = case runParser tokens' "spss" src of
  Left _ -> d <$ warn "spss" ("could not tokenise command: " <> T.take 40 src)
  Right ts -> case map up (take 3 ts) of
    (TWord a : TWord b : _)
      | kw "VARIABLE" a && kw "LABELS" b -> pure (varLabels (drop 2 ts))
      | kw "VALUE" a && kw "LABELS" b -> pure (valueLabels False (drop 2 ts))
      | kw "VARIABLE" a && kw "LEVEL" b -> pure (levels (drop 2 ts))
      | kw "MISSING" a && kw "VALUES" b -> pure (missing (drop 2 ts))
      | kw "INPUT" a && kw "PROGRAM" b -> pure d
      | kw "END" a && (kw "FILE" b || kw "INPUT" b) -> pure d
      | kw "NEW" a && kw "FILE" b -> pure d
      | kw "DATA" a && kw "LIST" b -> case break (== TSym '/') ts of
          (opts, _ : decls) | not (any (== TWord "FIXED") (map up opts)) -> pure (declare "F8.2" decls)
          _ -> d <$ warn "spss" "DATA LIST without FREE or LIST formats ignored"
      | kw "BEGIN" a && kw "DATA" b -> d <$ warn "spss" "inline data ignored"
      | kw "BEGIN" a && kw "PROGRAM" b -> d <$ warn "spss" "BEGIN PROGRAM block ignored"
      | kw "FILE" a && kw "LABEL" b, Nothing <- dTitle d -> pure d {dTitle = Just (T.unwords [t | TStr t <- drop 2 ts])}
    (TWord a : TWord b : TWord c : _)
      | kw "ADD" a && kw "VALUE" b && kw "LABELS" c -> pure (valueLabels True (drop 3 ts))
    (TWord a : _)
      | kw "NUMERIC" a -> pure (declare "F8.2" (drop 1 ts))
      | kw "STRING" a -> pure (declare "A8" (drop 1 ts))
      | kw "FORMATS" a -> pure (declare "F8.2" (drop 1 ts))
      | kw "MRSETS" a -> mrsets (drop 1 ts)
      | kw "TITLE" a -> pure d {dTitle = Just (T.unwords [t | TStr t <- drop 1 ts] `orIfEmpty` T.strip (T.drop 5 (T.stripStart src)))}
      | kw "EXECUTE" a -> pure d
    (TWord a : TWord b : _) | not (isVarLike b) -> d <$ warn "spss" (T.toUpper a <> " " <> T.toUpper b <> " command ignored")
    (TWord a : _) -> d <$ warn "spss" (T.toUpper a <> " command ignored")
    _ -> d <$ warn "spss" ("command ignored: " <> T.take 40 src)
  where
    up = \case
      TWord w -> TWord (T.toUpper w)
      t -> t
    -- SPSS accepts any abbreviation of three letters or more.
    kw full w = T.length w >= 3 && w `T.isPrefixOf` full
    orIfEmpty a b = if T.null a then b else a
    isVarLike w = T.any (\c -> c == '_' || isDigit c) w || T.any (`elem` ("=" :: String)) w
    -- SPSS names are case-insensitive; the first spelling is kept
    touch n0 f dd =
      let u = T.toUpper n0
          n = M.findWithDefault n0 u (dUpper dd)
          new = not (M.member u (dUpper dd))
       in dd
            { dRev = if new then n : dRev dd else dRev dd
            , dUpper = if new then M.insert u n (dUpper dd) else dUpper dd
            , dVars = M.alter (Just . f . fromMaybe emptyInfo) n (dVars dd)
            }
    -- a list of names, expanding @a TO c@ over the known order
    names = go []
      where
        go acc (TWord a : TWord t : TWord b : rest)
          | T.toUpper t == "TO" = go (reverse (range a b) ++ acc) rest
        go acc (TWord a : rest) | T.toUpper a /= "TO" = go (a : acc) rest
        go acc rest = (reverse acc, rest)
    range a b =
      let o = dOrder d
          ix x = lookup (T.toUpper x) (zip (map T.toUpper o) [0 :: Int ..])
       in case (ix a, ix b) of
            (Just i, Just j) -> take (j - i + 1) (drop i o)
            _ -> [a, b]
    splitSlash ts = case break (== TSym '/') ts of
      (x, []) -> [x]
      (x, _ : rest) -> x : splitSlash rest
    varLabels ts = foldl' (\dd (n, l) -> touch n (\v -> v {viLabel = Just l}) dd) d (pairs ts)
      where
        pairs = \case
          TWord n : TStr l : rest -> (n, l) : pairs rest
          _ : rest -> pairs rest
          [] -> []
    valueLabels add ts = foldl' apply d (splitSlash ts)
      where
        apply dd part =
          let (vs, rest) = names part
              labels = valuePairs rest
           in foldl' (\x n -> touch n (\v -> v {viValues = if add then merge (viValues v) labels else labels}) x) dd vs
        merge old new = [(k, l) | (k, l) <- old, k `notElem` map fst new] ++ new
    valuePairs = \case
      TNum v : TStr l : rest -> (v, l) : valuePairs rest
      TStr v : TStr l : rest -> (v, l) : valuePairs rest
      _ : rest -> valuePairs rest
      [] -> []
    levels ts = foldl' apply d (groupsParen ts)
      where
        apply dd (vs, inside) = case inside of
          [TWord l] -> foldl' (\x n -> touch n (\v -> v {viLevel = Just (T.toUpper l)}) x) dd vs
          _ -> dd
    missing ts = foldl' apply d (groupsParen ts)
      where
        apply dd (vs, inside) = foldl' (\x n -> touch n (\v -> v {viMissing = Just (render inside)}) x) dd vs
    -- @a b (X) c (Y)@ and @/a (X)@: each name list with the parenthesised
    -- tokens that follow it.
    groupsParen ts = case names (dropWhile (== TSym '/') ts) of
      ([], _) -> []
      (vs, TSym '(' : rest) ->
        let (inside, after) = break (== TSym ')') rest
         in (vs, inside) : groupsParen (drop 1 after)
      (_, rest) -> groupsParen (drop 1 rest)
    declare dflt ts = foldl' apply d (groupsParen' ts)
      where
        apply dd (vs, fmt) = foldl' (\x n -> touch n (\v -> v {viFormat = Just (fromMaybe (fromMaybe dflt (viFormat v)) fmt)}) x) dd vs
        groupsParen' xs = case names (dropWhile (== TSym '/') xs) of
          ([], _) -> []
          (vs, TSym '(' : rest) ->
            let (inside, after) = break (== TSym ')') rest
             in (vs, Just (T.toUpper (render inside))) : groupsParen' (drop 1 after)
          (vs, rest) -> (vs, Nothing) : groupsParen' (drop 1 rest)
    mrsets ts = foldl' (\m part -> m >>= \dd -> sub dd part) (pure d) (splitSlash ts)
    sub dd = \case
      TWord s : rest
        | T.toUpper s == "MDGROUP" ->
            let kv = keyValues rest
                get k = fromMaybe "" (lookup k kv >>= firstText)
             in pure
                  dd
                    { dSets =
                        dSets dd
                          ++ [ MrSet
                                 { mrName = fromMaybe (get "NAME") (T.stripPrefix "$" (get "NAME"))
                                 , mrLabel = get "LABEL"
                                 , mrVars = maybe [] (fst . names) (lookup "VARIABLES" kv)
                                 , mrValue = get "VALUE"
                                 , mrCatLabels = T.toUpper (get "CATEGORYLABELS")
                                 }
                             ]
                    }
        | T.toUpper s == "MCGROUP" -> dd <$ warn (fromMaybe "spss" (lookup "NAME" (keyValues rest) >>= firstText)) "multiple-category set kept as separate variables"
        | otherwise -> pure dd
      _ -> pure dd
    keyValues = \case
      TWord k : TSym '=' : rest ->
        let (v, more) = break isKey rest
         in (T.toUpper k, v) : keyValues more
      _ : rest -> keyValues rest
      [] -> []
    isKey = \case
      TWord w -> T.toUpper w `elem` ["NAME", "LABEL", "VARIABLES", "VALUE", "CATEGORYLABELS", "LABELSOURCE", "COUNTEDVALUES"]
      _ -> False
    firstText = \case
      (TStr t : _) -> Just t
      (TWord t : _) -> Just t
      (TNum t : _) -> Just t
      _ -> Nothing

-- | Tokens back to syntax, for keeping a MISSING VALUES list verbatim.
render :: [Tok] -> Text
render = T.replace " ," "," . T.unwords . map (\case TWord w -> w; TNum n -> n; TStr s -> quote s; TSym c -> T.singleton c)

setQuestion :: (Text -> VarInfo) -> MrSet -> Question
setQuestion info s =
  (question (mrName s) (mrLabel s) (QChoice Multiple Radio [Choice (code v) (plain (label v)) False False | v <- mrVars s]))
    { qAttrs =
        M.fromList $
          [("spss:value", String (mrValue s)) | mrValue s /= "1"]
            ++ [("spss:members", String (T.unwords (mrVars s))) | any (\v -> code v == v) (mrVars s)]
    }
  where
    code v = fromMaybe v (T.stripPrefix (mrName s <> "_") v)
    label v
      | mrCatLabels s == "COUNTEDVALUES" = fromMaybe v (lookup (mrValue s) (viValues (info v)))
      | otherwise = fromMaybe "" (viLabel (info v))

varQuestion :: Text -> VarInfo -> Conv Question
varQuestion n v = do
  let fmt = fromMaybe "" (viFormat v)
      letters = T.takeWhile isAlpha fmt
      width = fromMaybe 0 (readNum (T.takeWhile isDigit (T.dropWhile isAlpha fmt)))
      decimals = T.drop 1 (T.dropWhile (/= '.') fmt)
      isString = isStringFormat fmt
      vals = [(k, plain l) | (k, l) <- viValues v]
      level = viLevel v
  qt <-
    if
      | letters `elem` ["DATE", "ADATE", "EDATE", "SDATE", "JDATE", "QYR", "MOYR", "WKYR"] -> pure (QDate Date)
      | letters `elem` ["TIME", "DTIME", "MTIME"] -> pure (QDate Time)
      | letters `elem` ["DATETIME", "YMDHMS", "DATETIMEMS"] -> pure (QDate DateTime)
      | not (null vals), level == Just "SCALE", not isString -> do
          warn n "value labels on a scale variable dropped"
          pure (QNumber (if decimals `elem` ["", "0"] then Integer else Decimal))
      | not (null vals) -> pure $ case sort (viValues v) of
          [("0", "No"), ("1", "Yes")] | not isString -> QBoolean
          _
            | level == Just "ORDINAL", Just s <- scaleFromCodes vals -> s
            | otherwise -> QChoice Single Radio [Choice k l False False | (k, l) <- vals]
      | isString -> pure (QText (if width > 255 then LongText else ShortText))
      | otherwise -> pure (QNumber (if decimals `elem` ["", "0"] && not (T.null fmt) then Integer else Decimal))
  pure
    (question n "" qt)
      { qLabel = plain (fromMaybe "" (viLabel v))
      , qAttrs =
          M.fromList $
            [("spss:missing", String m) | Just m <- [viMissing v]]
              ++ [("spss:format", String fmt) | not (T.null fmt), keepsFormat qt, Just fmt /= defaultFormat qt]
              ++ [("spss:level", String l) | Just l <- [level], QChoice {} <- [qt], l /= "NOMINAL"]
      }

-- | The format the writer gives a type when none was read.
defaultFormat :: QType -> Maybe Text
defaultFormat = \case
  QText LongText -> Just "A1000"
  QText _ -> Just "A255"
  QNumber Integer -> Just "F8.0"
  QNumber Decimal -> Just "F8.2"
  QDate Date -> Just "DATE11"
  QDate Time -> Just "TIME8"
  QDate DateTime -> Just "DATETIME20"
  _ -> Nothing

isStringFormat :: Text -> Bool
isStringFormat f = T.takeWhile isAlpha (T.toUpper f) `elem` ["A", "AHEX"]

-- | Types whose SPSS format is not implied by their codes.
keepsFormat :: QType -> Bool
keepsFormat = \case
  QText _ -> True
  QNumber _ -> True
  QDate _ -> True
  _ -> False

------------------------------------------------------------------------------
-- Writing

-- | A variable ready to write.
data SVar = SVar
  { svName :: Text
  , svFormat :: Text
  , svLabel :: Text
  , svValues :: [(Text, Text)]
  , svLevel :: Maybe Text
  , svMissing :: Maybe Text
  }

writeSpss :: Form -> Conv BL.ByteString
writeSpss (Form meta items) = do
  qs <- concat <$> (flattenItems "spss" items >>= mapM expandQuestion)
  when (isJust (metaDescription meta)) $ warn "spss" "survey description dropped"
  parts <- dedupe <$> mapM varsOf qs
  let vars = concatMap fst parts
      sets = concatMap snd parts
      stmt name body = if null body then [] else [name] ++ zipWith (line (length body)) [1 ..] body ++ [""]
      line n i x = "  " <> (if i > (1 :: Int) then "/" else "") <> x <> (if i == n then "." else "")
      decl =
        ["INPUT PROGRAM."]
          ++ [(if isStringFormat (svFormat x) then "STRING " else "NUMERIC ") <> svName x <> " (" <> svFormat x <> ")." | x <- vars]
          ++ ["END FILE.", "END INPUT PROGRAM.", ""]
      labels = [svName x <> " " <> quote (svLabel x) | x <- vars, not (T.null (svLabel x))]
      values = [svName x <> " " <> T.unwords [val x k <> " " <> quote l | (k, l) <- svValues x] | x <- vars, not (null (svValues x))]
      levels = [T.unwords names <> " (" <> lvl <> ")" | lvl <- ["NOMINAL", "ORDINAL", "SCALE"], let names = [svName x | x <- vars, svLevel x == Just lvl], not (null names)]
      missing = [svName x <> " (" <> m <> ")" | x <- vars, Just m <- [svMissing x]]
      mrsets =
        [ "/MDGROUP NAME=$" <> n <> " LABEL=" <> quote l <> " CATEGORYLABELS=VARLABELS VARIABLES=" <> T.unwords vs <> " VALUE=" <> (if isJust (readNum v) then v else quote v)
        | (n, l, vs, v) <- sets
        ]
      header =
        ["* Encoding: UTF-8.", "* Data dictionary written by panform.", ""]
          ++ ["TITLE " <> quote (oneLine (i18nDefault t)) <> "." | Just t <- [metaTitle meta]]
          ++ ["" | isJust (metaTitle meta)]
      body =
        header
          ++ decl
          ++ stmt "VARIABLE LABELS" labels
          ++ stmt "VALUE LABELS" values
          ++ stmt "VARIABLE LEVEL" levels
          ++ stmt "MISSING VALUES" missing
          ++ (if null mrsets then [] else ["MRSETS"] ++ map ("  " <>) (init' mrsets) ++ ["  " <> last' mrsets <> "."])
  pure (BL.fromStrict (TE.encodeUtf8 (T.unlines (dropTrailingBlank body))))
  where
    val x k = if isStringFormat (svFormat x) then quote k else k
    init' xs = take (length xs - 1) xs
    last' xs = if null xs then "" else xs !! (length xs - 1)
    dropTrailingBlank = reverse . dropWhile T.null . reverse

-- | The variables for one question, and the multiple-response set (name,
-- label, members, counted value) when it is a multi-select.
varsOf :: Question -> Conv ([SVar], [(Text, Text, [Text], Text)])
varsOf q0 = do
  let n = spssName (qId q0)
  when (n /= qId q0) $ warn (qId q0) ("renamed to " <> n <> " (not a valid SPSS variable name)")
  varsOf' q0 {qId = n}

varsOf' :: Question -> Conv ([SVar], [(Text, Text, [Text], Text)])
varsOf' q = do
  lossWarnings (Loses True True True True True) q
  let texts = i18nDefault (qLabel q) : case qType q of
        QChoice _ _ cs -> map (i18nDefault . choiceLabel) cs
        QMatrix _ rs cs -> map (i18nDefault . choiceLabel) (rs ++ cs)
        QScale _ _ l h -> maybe [] (pure . i18nDefault) l ++ maybe [] (pure . i18nDefault) h
        _ -> []
  when (any (T.any (`elem` ("\n\r" :: String))) texts) $ warn (qId q) "line breaks in labels written as spaces"
  when (isJust (qValidation q >>= \v -> valMin v <|> valMax v)) $ warn (qId q) "value range dropped"
  when (T.length (i18nDefault (qLabel q)) > 256) $ warn (qId q) "label longer than 256 characters; SPSS truncates it"
  let base = SVar (qId q) "A255" (oneLine (i18nDefault (qLabel q))) [] Nothing (str "spss:missing")
      str k = case M.lookup k (qAttrs q) of
        Just (String m) -> Just m
        _ -> Nothing
      -- a format read from SPSS wins when it is of the same kind
      fmt f = case str "spss:format" of
        Just g | keepsFormat (qType q), isStringFormat g == isStringFormat f -> g
        _ -> f
      one v = pure ([v {svFormat = fmt (svFormat v)}], [])
      text f = one base {svFormat = f}
  case qType q of
    QText ShortText -> text "A255"
    QText LongText -> text "A1000"
    QText k -> text "A255" <* warn (qId q) (T.toLower (T.pack (show k)) <> " validation dropped")
    QNumber Integer -> one base {svFormat = "F8.0", svLevel = Just "SCALE"}
    QNumber Decimal -> one base {svFormat = "F8.2", svLevel = Just "SCALE"}
    QChoice Single _ [] -> text "A255" <* warn (qId q) "choice question with no options written as a string variable"
    QChoice Single style cs -> do
      choiceWarnings cs
      when (style /= Radio) $ warn (qId q) (T.toLower (tshow style) <> " display style dropped")
      one (coded base [(choiceValue c, i18nDefault (choiceLabel c)) | c <- cs]) {svLevel = Just (fromMaybe "NOMINAL" (str "spss:level"))}
    QChoice Multiple _ [] -> ([], []) <$ warn (qId q) "multi-select with no options dropped"
    QChoice Multiple _ cs -> do
      choiceWarnings cs
      let value = case M.lookup "spss:value" (qAttrs q) of
            Just (String v) -> v
            _ -> "1"
          (fmt, dichotomy)
            | isJust (readNum value) = ("F1.0", [("0", "Not selected"), (value, "Selected")])
            | otherwise = ("A" <> tshow (max 1 (T.length value)), [])
          -- member names read from SPSS are kept as they were
          names = case M.lookup "spss:members" (qAttrs q) of
            Just (String ms) | length (T.words ms) == length cs -> T.words ms
            _ -> [spssName (qId q <> "_" <> choiceValue c) | c <- cs]
          members = [SVar n fmt (oneLine (i18nDefault (choiceLabel c))) dichotomy (Just "NOMINAL") Nothing | (n, c) <- zip names cs]
      pure (members, [(qId q, oneLine (i18nDefault (qLabel q)), map svName members, value)])
    QScale lo hi l h -> one (coded base [(c, i18nDefault x) | (c, x) <- scaleCodes lo hi l h]) {svLevel = Just "ORDINAL"}
    QBoolean -> one base {svFormat = "F1.0", svValues = [("1", "Yes"), ("0", "No")], svLevel = Just "NOMINAL"}
    QDate Date -> text "DATE11"
    QDate Time -> text "TIME8"
    QDate DateTime -> text "DATETIME20"
    QHidden -> text "A255" <* warn (qId q) "hidden flag dropped"
    QCalculate _ -> one base {svFormat = "F8.2", svLevel = Just "SCALE"} <* warn (qId q) "calculation dropped; written as a numeric variable"
    QFile -> text "A255" <* warn (qId q) "file upload written as a string variable"
    QGeo -> text "A255" <* warn (qId q) "geopoint written as a string variable"
    QOther t -> text "A255" <* warn (qId q) ("type " <> t <> " written as a string variable")
    QMatrix {} -> pure ([], [])
    QRanking _ -> pure ([], [])
  where
    choiceWarnings cs = do
      when (any choiceOther cs) $ warn (qId q) "\"other, specify\" text box dropped"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive (none of the above) flag dropped"
    a <|> b = maybe b Just a

-- | Two variables may not share a name: later ones get @_2@, @_3@, ...
-- (multiple-response sets follow their members).
dedupe :: [([SVar], [(Text, Text, [Text], Text)])] -> [([SVar], [(Text, Text, [Text], Text)])]
dedupe = go M.empty
  where
    go _ [] = []
    go seen ((vs, sets) : rest) =
      let (seen', vs') = foldl step (seen, []) vs
          ren = zip (map svName vs) (map svName (reverse vs'))
          -- a set's members are the question's own variables, in order
          sets' = [(n, l, if ms == map svName vs then map svName (reverse vs') else [fromMaybe m (lookup m ren) | m <- ms], v) | (n, l, ms, v) <- sets]
       in (reverse vs', sets') : go seen' rest
    step (seen, acc) v =
      let base = svName v
          free k = let c = if k == (1 :: Int) then base else T.take (60 - T.length (tshow k)) base <> "_" <> tshow k in if M.member (T.toUpper c) seen then free (k + 1) else c
          n = free 1
       in (M.insert (T.toUpper n) () seen, v {svName = n} : acc)

-- | Numeric codes give a numeric variable wide enough for the longest;
-- anything else a string variable.
coded :: SVar -> [(Text, Text)] -> SVar
coded base cs =
  let numeric = all (\(c, _) -> isJust (readNum c) && T.strip c == c) cs && not (null cs)
      w = maximum (1 : map (T.length . fst) cs)
      decimals = maximum (0 : [T.length (T.drop 1 (T.dropWhile (/= '.') c)) | (c, _) <- cs])
   in base
        { svFormat = if numeric then "F" <> tshow (max w 1) <> "." <> tshow decimals else "A" <> tshow w
        , svValues = [(c, oneLine l) | (c, l) <- cs]
        }

-- | SPSS strings cannot span lines: line breaks become spaces.
oneLine :: Text -> Text
oneLine = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

quote :: Text -> Text
quote s = "'" <> T.replace "'" "''" s <> "'"

-- | A valid SPSS name: invalid characters become @_@, and a name that
-- cannot start a variable gets a @v@ prefix.
spssName :: Text -> Text
spssName n
  | validName n = n
  | otherwise =
      let body = T.dropWhileEnd (== '.') (T.map (\c -> if isAlphaNum c || c `elem` ("._$#@" :: String) then c else '_') n)
          fixed = case T.uncons body of
            Just (c, _) | isAlpha c || c `elem` ("@#$" :: String) -> body
            _ -> "v" <> body
       in if validName fixed then fixed else T.take 64 ("v_" <> fixed)

validName :: Text -> Bool
validName n = case T.uncons n of
  Just (c, rest) ->
    (isAlpha c || c `elem` ("@#$" :: String))
      && T.all (\x -> isAlphaNum x || x `elem` ("._$#@" :: String)) rest
      && not ("." `T.isSuffixOf` n)
      && T.toUpper n `notElem` ["ALL", "AND", "BY", "EQ", "GE", "GT", "LE", "LT", "NE", "NOT", "OR", "TO", "WITH"]
      && T.length n <= 64
  Nothing -> False
