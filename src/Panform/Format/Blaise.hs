-- | Blaise 4/5 data model source (Statistics Netherlands; CAPI/CATI in
-- official statistics). @FIELDS@ declares questions as
-- @name "text" : type@ with enumerations @(Yes "Yes", No "No")@,
-- @SET OF (...)@, @STRING[n]@, @OPEN@, ranges @0..120@ and @DATETYPE@;
-- @RULES@ asks them in order inside @IF ... THEN ... ENDIF@. Parallel
-- texts follow the order of @LANGUAGES =@.
module Panform.Format.Blaise (readBlaise, writeBlaise) where

import Control.Monad (forM, forM_, join, unless, void, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum)
import Data.List (groupBy, nub, nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import Numeric (showFFloat)
import Panform.AST hiding (choice)
import qualified Panform.AST as A
import Panform.ChoiceLogic
import Panform.Conv
import Panform.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------------------------
-- Syntax

type P = Parsec Void Text

-- | A text with an optional language identifier in front.
type Texts = [(Maybe Text, Text)]

data BType
  = BString (Maybe Int)
  | BOpen
  | BInteger
  | BReal
  | BRange Text Text
  | BEnum [Cat]
  | BSet (Maybe Text) [Cat]
  | BDate
  | BTime
  | BNamed Text
  | BArray BType

data Cat = Cat Text (Maybe Integer) Texts

data Field = Field
  { fName :: Text
  , fTag :: Maybe Text
  , fTexts :: Texts
  , fDesc :: Texts
  , fType :: BType
  , fAttrs :: [Text]
  }

data Stmt
  = SAsk Text
  | SAssign Text Text
  | SIf [(Text, [Stmt])] [Stmt]
  | SCheck Text

data Model = Model
  { mName :: Text
  , mTexts :: Texts
  , mLangs :: [Text]
  , mLangDescs :: [Maybe Text]
  , mTypes :: M.Map Text BType
  , mFields :: [Field]
  , mAux :: [Field]
  , mRules :: [Stmt]
  , mHasRules :: Bool
  , mSkipped :: [Text]
  }

sc :: P ()
sc = L.space space1 empty (L.skipBlockComment "{" "}")

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

kw :: Text -> P ()
kw k = lexeme . try $ void (string' k) <* notFollowedBy (satisfy isNameChar)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

sections :: [Text]
sections = ["FIELDS", "AUXFIELDS", "LOCALS", "RULES", "TYPE", "ENDMODEL", "LANGUAGES", "ATTRIBUTES", "PRIMARY", "SECONDARY", "SETTINGS", "BLOCK", "ENDBLOCK", "PARAMETERS", "USES", "INCLUDE"]

anySection :: P ()
anySection = choice (map kw sections)

name :: P Text
name = lexeme . try $ do
  notFollowedBy anySection
  c <- satisfy isAlpha
  T.pack . (c :) <$> many (satisfy isNameChar)

str :: P Text
str = lexeme $ do
  q <- char '"' <|> char '\''
  T.pack <$> manyTill (try (q <$ string (T.pack [q, q])) <|> anySingle) (try (char q <* notFollowedBy (char q)))

-- | A number as written (so @1.00@ keeps its decimals).
numText :: P Text
numText = lexeme . try $ do
  sign <- option "" ("-" <$ char '-')
  ds <- some digitChar
  frac <- optional (try (char '.' *> some digitChar))
  pure (T.pack (sign ++ ds ++ maybe "" ('.' :) frac))

texts :: P Texts
texts = many (((,) Nothing <$> str) <|> try ((,) . Just <$> name <*> str))

model :: P Model
model = do
  sc
  kw "DATAMODEL"
  n <- name
  ts <- texts
  go (Model n ts [] [] M.empty [] [] [] False [])
  where
    go m =
      choice
        [ kw "ENDMODEL" *> pure m
        , kw "LANGUAGES" *> symbol "=" *> (((,) <$> name <*> optional str) `sepBy1` symbol ",") >>= \ls -> go m {mLangs = map fst ls, mLangDescs = map snd ls}
        , kw "TYPE" *> many typeDef >>= \ts -> go m {mTypes = mTypes m <> M.fromList ts}
        , kw "FIELDS" *> many fieldDefs >>= \fs -> go m {mFields = mFields m ++ concat fs}
        , kw "AUXFIELDS" *> many fieldDefs >>= \fs -> go m {mAux = mAux m ++ concat fs}
        , kw "LOCALS" *> many fieldDefs *> go m {mSkipped = mSkipped m ++ ["LOCALS"]}
        , kw "RULES" *> many stmt >>= \ss -> go m {mRules = mRules m ++ ss, mHasRules = True}
        , do
            k <- choice (map (\s -> s <$ kw s) ["ATTRIBUTES", "PRIMARY", "SECONDARY", "SETTINGS", "PARAMETERS", "USES", "INCLUDE"])
            _ <- manyTill anySingle (lookAhead (try (eol *> sc *> anySection)) <|> eof)
            sc
            go m {mSkipped = mSkipped m ++ [k]}
        ]
    typeDef = (,) <$> name <* symbol "=" <*> btype

fieldDefs :: P [Field]
fieldDefs = do
  ns <- name `sepBy1` symbol ","
  tag <- optional (between (symbol "(") (symbol ")") (T.pack <$> lexeme (some (satisfy (\c -> isNameChar c)))))
  ts <- texts
  desc <- option [] (symbol "/" *> texts)
  _ <- symbol ":"
  t <- btype
  as <- many (try (symbol "," *> (T.toUpper <$> name)))
  pure [Field n tag ts desc t as | n <- ns]

btype :: P BType
btype =
  choice
    [ kw "STRING" *> (BString <$> optional (between (symbol "[") (symbol "]") (lexeme L.decimal)))
    , kw "OPEN" *> pure BOpen
    , kw "INTEGER" *> (BInteger <$ optional (between (symbol "[") (symbol "]") (lexeme (L.decimal :: P Int))))
    , kw "REAL" *> (BReal <$ optional (between (symbol "[") (symbol "]") (numText `sepBy` symbol ",")))
    , kw "DATETYPE" *> pure BDate
    , kw "TIMETYPE" *> pure BTime
    , kw "SET" *> (setOf <$> optional (between (symbol "[") (symbol "]") numText) <* kw "OF" <*> btype)
    , kw "ARRAY" *> between (symbol "[") (symbol "]") (many (satisfy (/= ']'))) *> kw "OF" *> (BArray <$> btype)
    , BEnum <$> between (symbol "(") (symbol ")") (cat `sepBy1` symbol ",")
    , try (BRange <$> numText <* symbol ".." <*> numText)
    , BNamed <$> name
    ]
  where
    setOf n = \case
      BEnum cs -> BSet n cs
      BNamed t -> BSet n [Cat ("\0" <> t) Nothing []]
      other -> other
    cat = Cat <$> name <*> optional (between (symbol "(") (symbol ")") (lexeme L.decimal)) <*> texts

stmt :: P Stmt
stmt =
  choice
    [ ifStmt
    , try (SAssign <$> dotted <* symbol ":=" <*> (T.strip . T.pack <$> manyTill anySingle (void eol <|> eof)) <* sc)
    , try checkStmt
    , (kw "CHECK" <|> kw "SIGNAL") *> stmt
    , SAsk <$> dotted
    ]
  where
    ifStmt = do
      kw "IF"
      c <- condText
      body <- many stmt
      elifs <- many (kw "ELSEIF" *> ((,) <$> condText <*> many stmt))
      els <- option [] (kw "ELSE" *> many stmt)
      kw "ENDIF"
      pure (SIf ((c, body) : elifs) els)
    condText = do
      t <- T.pack <$> manyTill anySingle (try (sc *> kw "THEN"))
      -- A condition may carry a description text before THEN.
      pure (T.strip (stripDesc (T.strip t)))
    stripDesc t
      | "\"" `T.isSuffixOf` t
      , (pre, _) <- T.breakOnEnd "\"" (T.dropEnd 1 t)
      , not (T.null pre)
      , Just (_, c) <- T.unsnoc (T.stripEnd (T.dropEnd 1 pre))
      , c `notElem` ("=<>+-*/(,[" :: String) =
          T.dropEnd 1 pre
      | otherwise = t
    dotted = lexeme . try $ do
      notFollowedBy (choice (map kw ["IF", "ELSE", "ELSEIF", "ENDIF", "THEN", "CHECK", "SIGNAL"]))
      notFollowedBy anySection
      c <- satisfy isAlpha
      T.pack . (c :) <$> many (satisfy (\x -> isNameChar x || x == '.' || x == '[' || x == ']'))
    -- A check or signal: a condition followed by its message.
    checkStmt = do
      notFollowedBy (choice (map kw ["IF", "ELSE", "ELSEIF", "ENDIF"]))
      e <- T.pack <$> someTill (satisfy (/= '"')) (lookAhead (char '"'))
      let up = T.toUpper e
      when (any (`T.isInfixOf` up) ["ENDIF", "THEN", ":="] || T.any (== '\n') (T.strip e) || not (T.any (`elem` ("=<>" :: String)) e)) $ fail "not a check"
      _ <- str
      pure (SCheck (T.strip e))

------------------------------------------------------------------------------
-- Reading

readBlaise :: BL.ByteString -> Conv Form
readBlaise bs = do
  let src = TE.decodeUtf8Lenient (BL.toStrict bs)
  m <- either (Left . errorBundlePretty) Right (parse model "Blaise" src) `orFail` "Blaise data model"
  forM_ (mSkipped m) $ \k -> warn "blaise" (k <> " section ignored")
  let langs = mLangs m
      tags = zipWith langTag langs (mLangDescs m ++ repeat Nothing)
      i18n = textsI18n (zip (map T.toUpper langs) tags)
  fieldItems <- mapM (fieldQuestion m i18n False) (mFields m)
  auxItems <- mapM (fieldQuestion m i18n True) (mAux m)
  r <- walk langs (mRules m)
  let byName = M.fromList [(T.toLower (itemId i), i) | i <- fieldItems ++ auxItems]
      hasRules = mHasRules m
      askedOrder = nub (map T.toLower (rOrder r))
      -- A field asked in several places is on the route when any of them is.
      conds = M.fromListWith (\new old -> EBin Or <$> old <*> new) [(T.toLower n, c) | (n, c) <- rAsks r]
      calcs = M.fromList [(T.toLower n, (e, c)) | (n, e, c) <- rCalcs r]
      repeated xs = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(x, 1) | x <- xs]))
      placed = S.fromList askedOrder
      finish i = case i of
        ItemQuestion q ->
          let k = T.toLower (qId q)
              t = case (M.lookup k calcs, qType q) of
                (Just (e, _), _) -> QCalculate e
                (Nothing, QText _) | hasRules, S.notMember k placed -> QHidden
                (_, other) -> other
              rel = case M.lookup k calcs of
                Just (_, c) -> c
                _ -> join (M.lookup k conds)
           in ItemQuestion q {qType = t, qRelevant = if t == QHidden then Nothing else rel}
        ItemNote n t _ -> ItemNote n t (join (M.lookup (T.toLower n) conds))
        other -> other
      ordered =
        [finish i | k <- askedOrder, Just i <- [M.lookup k byName]]
          ++ [finish i | i <- fieldItems ++ auxItems, S.notMember (T.toLower (itemId i)) placed]
  forM_ [n | (n, _) <- rAsks r, M.notMember (T.toLower n) byName] $ \n -> warn n "rules refer to a field that is not declared"
  forM_ (repeated [T.toLower n | (n, _) <- rAsks r]) $ \k ->
    warn (maybe k itemId (M.lookup k byName)) "asked more than once in the rules; kept where it is first asked, shown when any of those asks applies"
  forM_ (repeated [T.toLower n | (n, _, _) <- rCalcs r]) $ \k ->
    warn (maybe k itemId (M.lookup k byName)) "assigned more than once in the rules; only the last assignment and its condition are kept"
  when hasRules $ forM_ [qId q | ItemQuestion q <- ordered, S.notMember (T.toLower (qId q)) placed, M.notMember (T.toLower (qId q)) calcs, not (isHidden q)] $ \n -> warn n "never asked in the rules"
  let meta =
        emptyMeta
          { metaId = Just (mName m)
          , metaTitle = i18n (mTexts m)
          , metaLanguages = tags
          }
      s = Form meta ordered
      idx = choiceIndex False s
      names = M.fromList [((qId q, T.toLower n), v) | q <- questions s, Just (String cs) <- [M.lookup "blaise:names" (qAttrs q)], (n, v) <- decodePairs cs]
      value q n = M.findWithDefault n (q, T.toLower n) names
      enumRefs = mapExpr $ \case
        EBin op (ERef q) (ERef n) | op `elem` [Eq, Neq], M.member (q, T.toLower n) names -> EBin op (ERef q) (ELit (LStr (value q n)))
        e -> e
  pure (mapSurveyLogic (fromSelected idx value . enumRefs) s)
  where
    isHidden q = qType q == QHidden

-- | A language's tag: its description when that is already a tag (as this
-- writer emits), else the identifier (@ENG@ becomes @eng@).
langTag :: Text -> Maybe Text -> Lang
langTag i desc = case desc of
  Just d | isTag d || langId d == i -> d
  _ -> T.replace "_" "-" (T.toLower i)
  where
    isTag d = case T.splitOn "-" d of
      (p : rest) -> T.length p `elem` [2, 3] && T.all isAlpha p && all (\x -> not (T.null x) && T.length x <= 8 && T.all isAlphaNum x) rest
      [] -> False

-- | Positional texts follow the language order; prefixed ones name theirs.
textsI18n :: [(Text, Lang)] -> Texts -> Maybe I18n
textsI18n tagOf ts =
  let positional = [t | (Nothing, t) <- ts]
      named = [(T.toUpper l, t) | (Just l, t) <- ts]
      langs = map fst tagOf
      byLang = zip langs positional ++ named
      langTag' l = fromMaybe (T.toLower l) (lookup l tagOf)
   in case (langs, positional) of
        (l0 : _, _) -> case lookup (T.toUpper l0) byLang of
          Just d -> Just (I18n d (M.fromList [(langTag' l, t) | (l, t) <- byLang, l /= T.toUpper l0, not (T.null t)]))
          Nothing -> Nothing
        ([], d : _) -> Just (plain d)
        _ -> Nothing

fieldQuestion :: Model -> (Texts -> Maybe I18n) -> Bool -> Field -> Conv Item
fieldQuestion m i18n aux f = do
  let n = fName f
      label = fromMaybe (plain "") (i18n (fTexts f))
      attrs = filter (`notElem` ["EMPTY", "DK", "RF", "DONTKNOW", "REFUSAL", "NODK", "NORF", "NOEMPTY"]) (fAttrs f)
  unless (null (mLangs m) || length [() | (Nothing, _) <- fTexts f] <= length (mLangs m)) $ warn n "extra texts dropped"
  when (null (mLangs m) && length [() | (Nothing, _) <- fTexts f] > 1) $ warn n "extra texts dropped (no LANGUAGES declared)"
  forM_ attrs $ \a -> warn n ("attribute " <> a <> " dropped")
  forM_ [a | a <- fAttrs f, a `elem` ["DK", "RF", "DONTKNOW", "REFUSAL"]] $ \a -> warn n ("special answer " <> a <> " dropped")
  (qt, names) <- typeOf [] n (fType f)
  let q =
        (question n "" qt)
          { qLabel = label
          , qRequired = "EMPTY" `notElem` fAttrs f
          , qValidation = case fType f of
              BRange lo hi -> Just (Validation (readD lo) (readD hi) Nothing Nothing)
              _ -> Nothing
          , qAttrs =
              M.fromList $
                [("blaise:tag", String t) | Just t <- [fTag f]]
                  ++ [("blaise:description", String (i18nDefault d)) | Just d <- [i18n (fDesc f)]]
                  ++ [("blaise:names", String (encodePairs names)) | not (null names)]
                  ++ [("blaise:length", String (T.pack (show k))) | BString (Just k) <- [fType f], k /= 255]
          }
  case (aux, fType f) of
    (True, BString (Just 1)) -> pure (ItemNote n label Nothing)
    (True, _) -> do
      warn n "auxiliary field read as a question"
      pure (ItemQuestion q)
    _ -> pure (ItemQuestion q)
  where
    readD t = case reads (T.unpack t) :: [(Double, String)] of
      [(d, "")] -> Just d
      _ -> Nothing
    -- The named types being resolved, so that a type defined through
    -- itself ends in a warning.
    typeOf seen n = \case
      BString _ -> pure (QText ShortText, [])
      BOpen -> pure (QText LongText, [])
      BInteger -> pure (QNumber Integer, [])
      BReal -> pure (QNumber Decimal, [])
      BRange lo hi -> pure (QNumber (if T.any (== '.') (lo <> hi) then Decimal else Integer), [])
      BDate -> pure (QDate Date, [])
      BTime -> pure (QDate Time, [])
      BEnum cs -> (\(chs, ns) -> (QChoice Single Radio chs, ns)) <$> cats cs
      BSet mx [Cat t Nothing []] | Just ty <- T.stripPrefix "\0" t -> typeOf seen n (BNamed ty) >>= \case
        (QChoice _ st chs, ns) -> (QChoice Multiple st chs, ns) <$ maxNote mx
        other -> pure other
      BSet mx cs -> (\(chs, ns) -> (QChoice Multiple Radio chs, ns)) <$> (maxNote mx *> cats cs)
      BNamed t
        | T.toLower t `elem` seen -> (QOther t, []) <$ warn n ("type " <> t <> " is defined in terms of itself")
        | otherwise -> case M.lookup (T.toLower t) typesByName of
            Just bt -> typeOf (T.toLower t : seen) n bt
            Nothing -> (QOther t, []) <$ warn n ("type " <> t <> " is not declared")
      BArray bt -> warn n "array read as a single field" >> typeOf seen n bt
    typesByName = M.mapKeys T.toLower (mTypes m)
    maxNote mx = forM_ mx $ \k -> warn (fName f) ("limit of " <> k <> " answers dropped")
    cats cs = do
      let coded = any (\(Cat _ c _) -> isJust c) cs
          codes = drop 1 (scanl (\prev (Cat _ c _) -> fromMaybe (prev + 1) c) 0 cs)
          chs = [A.Choice (if coded then T.pack (show k) else cn) (fromMaybe (plain cn) (i18n ts)) False False | (Cat cn _ ts, k) <- zip cs codes]
      pure (chs, [(cn, choiceValue ch) | (Cat cn _ _, ch) <- zip cs chs])

encodePairs :: [(Text, Text)] -> Text
encodePairs = T.intercalate " " . map (\(a, b) -> a <> "=" <> b)

decodePairs :: Text -> [(Text, Text)]
decodePairs = map (fmap (T.drop 1) . T.breakOn "=") . T.words

data Walk = Walk
  { rAsks :: [(Text, Maybe Expr)]
  , rCalcs :: [(Text, Expr, Maybe Expr)]
  , rOrder :: [Text]
  -- ^ Fields asked or computed, in rule order.
  }

-- | Rules in order, each with the conjunction of its enclosing conditions.
walk :: [Text] -> [Stmt] -> Conv Walk
walk _ = go Nothing
  where
    go ctx ss = mconcat' <$> mapM (one ctx) ss
    one ctx = \case
      SAsk n -> pure (Walk [(method n, ctx)] [] [method n])
      SAssign n e -> pure (Walk [] [(n, parseOrRaw blaise e, ctx)] [n])
      SCheck c -> Walk [] [] [] <$ warn "rules" ("check dropped: " <> c)
      SIf branches els -> do
        let conds = [parseOrRaw blaise c | (c, _) <- branches]
            guards = [andAll (map ENot (take i conds) ++ [c]) | (i, c) <- zip [0 ..] conds]
        bs <- forM (zip guards branches) $ \(g, (_, body)) -> go (andMaybe ctx (Just g)) body
        e <- go (andMaybe ctx (Just (andAll (map ENot conds)))) els
        pure (mconcat' (bs ++ [e]))
    andAll = foldr1 (EBin And)
    mconcat' ws = Walk (concatMap rAsks ws) (concatMap rCalcs ws) (concatMap rOrder ws)
    method n = case T.breakOnEnd "." n of
      (pre, m) | not (T.null pre), T.toUpper m `elem` ["ASK", "SHOW", "KEEP"] -> T.dropEnd 1 pre
      _ -> n

------------------------------------------------------------------------------
-- Writing

writeBlaise :: Form -> Conv BL.ByteString
writeBlaise s0@(Form meta _) = do
  s@(Form _ items) <- expandMatrices s0 >>= legalIds "Q" blaiseName
  let used = surveyLangs s
      langs0 = case metaLanguages meta of
        [] | null used -> []
        [] -> "en" : used
        ls -> nub (ls ++ used)
      langs = nubBy (\a b -> langId a == langId b) langs0
      others = drop 1 langs
  when (null (metaLanguages meta) && not (null used)) $ warn "blaise" "default language assumed to be en"
  when (isJust (metaDescription meta)) $ warn "blaise" "survey description dropped"
  forM_ [g | ItemGroup g <- allItems items] $ \g -> warn (groupId g) "group flattened"
  let idx = choiceIndex True s
      names = M.fromList [((qId q, choiceValue c), n) | q <- questions s, Just (_, cs) <- [M.lookup (qId q) idx], (c, n) <- zip cs (enumNames q cs)]
      name q v = M.findWithDefault v (q, v) names
      single q = maybe False ((== Single) . fst) (M.lookup q idx)
      enumEq = \case
        ESelected q v | single q -> ERaw "blaise" (q <> " = " <> v)
        ENot (ESelected q v) | single q -> ERaw "blaise" (q <> " <> " <> v)
        ENot x -> ENot (enumEq x)
        EBin o a b -> EBin o (enumEq a) (enumEq b)
        ECall f xs -> ECall f (map enumEq xs)
        e -> e
      flat = flattenGroups items
      Form _ items' = mapSurveyLogic (enumEq . toSelected idx name) (Form meta flat)
      modelName = blaiseName (fromMaybe "Survey" (metaId meta))
  -- Unasked (hidden) fields last, where the reader puts them.
  let unasked q = case qType q of
        QHidden -> True
        QCalculate e -> either (const True) (const False) (printExpr blaise e)
        _ -> False
      (hiddenQs, askedQs) = partitionOn unasked [q | ItemQuestion q <- items']
  fields <- concat <$> mapM (fieldLines others) (askedQs ++ hiddenQs)
  let notes = [(i, t) | ItemNote i t _ <- items']
  rules <- ruleLines items'
  unless (all (\case ItemRaw _ _ -> False; _ -> True) items') $ warn "blaise" "raw content dropped"
  let out =
        ["DATAMODEL " <> modelName <> maybe "" ((" " <>) . textsOut others) (metaTitle meta)]
          ++ ["LANGUAGES = " <> T.intercalate ", " [langId l <> " " <> quote l | l <- langs] | not (null langs)]
          ++ [""]
          ++ (if null notes then [] else ["AUXFIELDS"] ++ ["  " <> i <> " " <> textsOut others t <> " : STRING[1], EMPTY" | (i, t) <- notes] ++ [""])
          ++ ["FIELDS"]
          ++ fields
          ++ ["", "RULES"]
          ++ map ("  " <>) rules
          ++ ["ENDMODEL"]
  pure (BL.fromStrict (TE.encodeUtf8 (T.unlines out)))

langId :: Lang -> Text
langId = T.toUpper . T.map (\c -> if isAlphaNum c then c else '_')

quote :: Text -> Text
quote t = "\"" <> T.replace "\"" "\"\"" t <> "\""

textsOut :: [Lang] -> I18n -> Text
textsOut others (I18n d tr) = T.unwords (quote d : [langId l <> " " <> quote t | l <- others, Just t <- [M.lookup l tr]])

-- | Category names: the values themselves when they are identifiers, or
-- @c1@, @c2@... with explicit codes when every value is an ascending integer.
enumNames :: Question -> [Choice] -> [Text]
enumNames q cs = dedupe [] [M.findWithDefault (enumName (choiceValue c)) (choiceValue c) kept | c <- cs]
  where
    kept = case M.lookup "blaise:names" (qAttrs q) of
      Just (String t) -> M.fromList [(v, n) | (n, v) <- decodePairs t, blaiseName n == n]
      _ -> M.empty
    enumName v = blaiseName (if isCanonical v then "c" <> v else v)
    dedupe _ [] = []
    dedupe seen (n : ns) =
      let n' = fromMaybe n (listToMaybe [c | c <- n : [n <> "_" <> T.pack (show k) | k <- [2 :: Int ..]], T.toLower c `notElem` seen])
       in n' : dedupe (T.toLower n' : seen) ns

-- | An integer written the way it reads back.
isCanonical :: Text -> Bool
isCanonical v = isInteger v && T.pack (show (read (T.unpack v) :: Integer)) == v

-- | A legal Blaise identifier; legal ones are returned unchanged.
blaiseName :: Text -> Text
blaiseName v =
  let clean = T.map (\c -> if isAlphaNum c || c == '_' then c else '_') v
      lettered = case T.uncons clean of
        Just (c, _) | isAlpha c -> clean
        _ -> "Q" <> clean
   in if T.toUpper lettered `elem` reserved then lettered <> "_" else lettered
  where
    reserved = sections ++ ["IF", "THEN", "ELSE", "ELSEIF", "ENDIF", "AND", "OR", "NOT", "IN", "EMPTY", "RESPONSE", "DK", "RF", "DONTKNOW", "REFUSAL", "NODK", "NORF", "NOEMPTY", "CHECK", "SIGNAL", "STRING", "OPEN", "INTEGER", "REAL", "SET", "OF", "ARRAY", "DATETYPE", "TIMETYPE", "DATAMODEL", "TRUE", "FALSE", "DIV", "MOD"]

-- | Blaise has no grids: each matrix row becomes a question of its own,
-- before ids are made legal and unique.
expandMatrices :: Form -> Conv Form
expandMatrices (Form m items) = Form m <$> mapM go items
  where
    go = \case
      ItemGroup g -> (\ks -> ItemGroup g {groupItems = ks}) <$> mapM go (groupItems g)
      ItemQuestion q | QMatrix sel rs cs <- qType q -> do
        warn (qId q) "matrix expanded into one field per row"
        pure . ItemGroup $
          Group (qId q) Section Nothing Nothing
            [ ItemQuestion
                q
                  { qId = qId q <> "_" <> choiceValue r
                  , qLabel = rowLabel (qLabel q) (choiceLabel r)
                  , qType = if null cs then QText ShortText else QChoice sel Radio cs
                  }
            | r <- rs
            ]
            M.empty
      i -> pure i
    rowLabel (I18n d tr) (I18n rd rtr) = I18n (d <> ": " <> rd) (M.intersectionWith (\a b -> a <> ": " <> b) tr rtr)

-- | The field a matrix row is expanded into.
rowField :: Question -> Choice -> Text
rowField q r = blaiseName (qId q <> "_" <> choiceValue r)

partitionOn :: (a -> Bool) -> [a] -> ([a], [a])
partitionOn p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])

enumItems :: Question -> [Lang] -> [Choice] -> Conv Text
enumItems q others cs = do
  let at = qId q
  let vals = map choiceValue cs
      ints = map (\v -> read (T.unpack v) :: Integer) vals
      coded = all isCanonical vals && and (zipWith (<) ints (drop 1 ints))
  forM_ (zip cs (enumNames q cs)) $ \(c, n) -> do
    unless (n == choiceValue c || coded) $ warn at ("choice value " <> choiceValue c <> " written as " <> n)
    when (choiceOther c) $ warn at ("other-specify text box on " <> choiceValue c <> " dropped")
    when (choiceExclusive c) $ warn at ("exclusive flag on " <> choiceValue c <> " dropped")
  let item (c, n) = n <> (if coded && not (isIdent (choiceValue c)) then " (" <> choiceValue c <> ")" else "") <> " " <> textsOut others (choiceLabel c)
  pure ("(" <> T.intercalate ",\n     " (map item (zip cs (enumNames q cs))) <> ")")

fieldLines :: [Lang] -> Question -> Conv [Text]
fieldLines others q = do
  let note = warn (qId q)
      tag = case M.lookup "blaise:tag" (qAttrs q) of
        Just (String t) -> " (" <> t <> ")"
        _ -> ""
      desc = case M.lookup "blaise:description" (qAttrs q) of
        Just (String t) -> " / " <> quote t
        _ -> ""
      len = case M.lookup "blaise:length" (qAttrs q) of
        Just (String n) -> n
        _ -> "255"
      range k = case qValidation q of
        Just (Validation (Just lo) (Just hi) _ _) -> num k lo <> ".." <> num k hi
        Just (Validation lo hi _ _) | isJust lo || isJust hi -> if k == Integer then "INTEGER" else "REAL"
        _ -> if k == Integer then "INTEGER" else "REAL"
  when (isJust (qHint q)) $ note "hint dropped"
  when (isJust (qDefault q)) $ note "default value dropped"
  forM_ (qValidation q) $ \v -> do
    when (isJust (valExpr v)) $ note "validation expression dropped"
    when (isJust (valMessage v)) $ note "validation message dropped"
    case (valMin v, valMax v, qType q) of
      (Just _, Nothing, QNumber _) -> note "one-sided range dropped"
      (Nothing, Just _, QNumber _) -> note "one-sided range dropped"
      _ -> pure ()
  t <- case qType q of
    QText k -> do
      when (k `elem` [Email, Url, Phone]) $ note "input kind written as plain text"
      pure (if k == LongText then "OPEN" else "STRING[" <> len <> "]")
    QNumber k -> pure (range k)
    QRanking [] -> note "ranking without options written as a string" >> pure ("STRING[" <> len <> "]")
    QChoice _ _ [] -> note "choice without options written as a string" >> pure ("STRING[" <> len <> "]")
    QChoice sel style cs -> do
      when (style /= Radio) $ note "choice style dropped"
      e <- enumItems q others cs
      pure ((if sel == Multiple then "SET OF " else "") <> e)
    QBoolean -> note "boolean written as a yes/no enumeration" >> enumItems q others boolChoices
    QRanking cs -> note "ranking written as a set" >> ("SET OF " <>) <$> enumItems q others cs
    QScale lo hi l h -> do
      note (if isJust l || isJust h then "scale written as a range; end labels dropped" else "scale written as a range")
      pure (T.pack (show lo) <> ".." <> T.pack (show hi))
    QDate k -> do
      when (k == DateTime) $ note "date-time written as a date"
      pure (if k == Time then "TIMETYPE" else "DATETYPE")
    QMatrix {} -> pure ""
    QFile -> note "file upload written as a string" >> pure "STRING[255]"
    QGeo -> note "geopoint written as a string" >> pure "STRING[255]"
    QHidden -> pure ("STRING[" <> len <> "]")
    QCalculate _ -> pure ("STRING[" <> len <> "]")
    QOther o -> note ("type " <> o <> " written as a string") >> pure "STRING[255]"
  let empty = if qRequired q then "" else ", EMPTY"
  case qType q of
    QMatrix sel rs cs -> do
      note "matrix expanded into one field per row"
      e <- if null cs then ("STRING[" <> len <> "]") <$ note "matrix without columns written as strings" else enumItems q others cs
      pure
        [ "  " <> rowField q r <> " " <> textsOut others (rowLabel (qLabel q) (choiceLabel r)) <> "\n    : " <> (if sel == Multiple && not (null cs) then "SET OF " else "") <> e <> empty
        | r <- rs
        ]
    _ -> pure ["  " <> qId q <> tag <> " " <> textsOut others (qLabel q) <> desc <> "\n    : " <> t <> empty]
  where
    num k x
      | k == Decimal = T.pack (showFFloat Nothing x "")
      | otherwise = T.pack (show (round x :: Integer))
    rowLabel (I18n d tr) (I18n rd rtr) = I18n (d <> ": " <> rd) (M.intersectionWith (\a b -> a <> ": " <> b) tr rtr)

-- | Rules in item order; consecutive items under the same condition share
-- one IF.
ruleLines :: [Item] -> Conv [Text]
ruleLines items = do
  steps <- concat <$> mapM step items
  let runs = groupBy (\(a, _) (b, _) -> a == b) steps
  concat <$> forM runs (\run -> case run of
    ((c, _) : _) -> do
      ct <- printLogic blaise "rules" c
      let body = map snd run
      pure $ case ct of
        Just t -> ["IF " <> t <> " THEN"] ++ map ("  " <>) body ++ ["ENDIF"]
        Nothing -> body
    [] -> pure [])
  where
    step = \case
      ItemNote i _ rel -> pure [(rel, i)]
      ItemQuestion q -> case qType q of
        QHidden -> pure []
        QCalculate e -> do
          rhs <- printLogic blaise (qId q) (Just e)
          pure [(qRelevant q, qId q <> " := " <> r) | Just r <- [rhs]]
        QMatrix _ rs _ -> pure [(qRelevant q, rowField q r) | r <- rs]
        _ -> pure [(qRelevant q, qId q)]
      _ -> pure []
