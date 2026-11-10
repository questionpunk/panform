-- | IBM SPSS Dimensions / Unicom Intelligence interview scripts: the
-- declarative @Metadata ... End Metadata@ section (mrScriptMetadata:
-- @q1 "Label" categorical [1..1] { yes "Yes", no "No" };@, @text@, @long@,
-- @double@, @date@, @boolean@, @info@, @loop ... fields (...) expand grid@,
-- @page(...)@) and a procedural @Routing(Web)@ section, of which the reader
-- understands @q.Ask()@, @If ... Then@ and @q.MustAnswer = False@.
module Panform.Format.MrScript (readMrScript, writeMrScript) where

import Control.Monad (forM, forM_, join, unless, void, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import Panform.AST hiding (choice)
import Panform.ChoiceLogic
import Panform.Conv
import Panform.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------------------------
-- Syntax

type P = Parsec Void Text

data Field = Field
  { fName :: Text
  , fLabels :: [(Maybe Text, Text)]
  -- ^ Label texts, by area name ('Nothing' is the default language).
  , fDropdown :: Bool
  , fDropped :: [Text]
  -- ^ Constructs read but not carried (properties, templates, ...).
  , fType :: FType
  }

data FType
  = FCat (Maybe Double, Maybe Double) [Cat]
  | FText (Maybe Double, Maybe Double)
  | FLong (Maybe Double, Maybe Double)
  | FDouble (Maybe Double, Maybe Double)
  | FDate
  | FBool
  | FInfo
  | FLoop (Either (Maybe Double, Maybe Double) [Cat]) [Field]
  | FPage [Text]
  | FBlock [Field]
  | FArea Text
  | FRaw Text

data Cat = Cat
  { catName :: Text
  , catLabels :: [(Maybe Text, Text)]
  , catKeywords :: [Text]
  }

sc :: P ()
sc = L.space space1 (L.skipLineComment "'") empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` ("_@$#" :: String)

name :: P Text
name = lexeme . try $ do
  c <- satisfy (\x -> x == '_' || x == '@' || (isNameChar x && not (isDigit x)))
  T.pack . (c :) <$> many (satisfy isNameChar)

kw :: Text -> P ()
kw k = lexeme . try $ void (string' k) <* notFollowedBy (satisfy isNameChar)

str :: P Text
str = lexeme $ do
  _ <- char '"'
  T.pack <$> manyTill (try ('"' <$ string "\"\"") <|> anySingle) (try (char '"' <* notFollowedBy (char '"')))

num :: P Double
num = lexeme . try $ do
  sign <- option id (negate <$ char '-')
  sign <$> (try L.float <|> fromIntegral <$> (L.decimal :: P Integer))

range :: P (Maybe Double, Maybe Double)
range = between (symbol "[") (symbol "]") $ do
  lo <- optional num
  dots <- optional (symbol "..")
  hi <- if isJust dots then optional num else pure lo
  pure (lo, hi)

-- | Anything up to the matching close bracket, kept as text.
balanced :: Text -> Text -> P Text
balanced o c = lexeme $ do
  _ <- string o
  inner <- go (1 :: Int) []
  pure (T.pack (reverse inner))
  where
    go 0 acc = pure (drop 1 acc)
    go n acc = do
      x <- anySingle
      let n'
            | T.singleton x == o = n + 1
            | T.singleton x == c = n - 1
            | otherwise = n
      go n' (x : acc)

data Prop = PLabel (Maybe Text) Text | PDrop Bool Text

labelProps :: P [Prop]
labelProps =
  many $
    choice
      [ PLabel Nothing <$> str
      , try (PLabel . Just <$> name <* symbol ":" <*> str)
      , PDrop False "properties" <$ balanced "[" "]"
      , try (kw "style" *> (styleProp <$> balanced "(" ")"))
      , try (kw "templates" *> (PDrop False "templates" <$ balanced "(" ")"))
      ]
  where
    styleProp t = PDrop ("droplist" `T.isInfixOf` T.toLower t) "style"

collect :: [Prop] -> ([(Maybe Text, Text)], Bool, [Text])
collect ps =
  ( [(a, t) | PLabel a t <- ps]
  , or [d | PDrop d _ <- ps]
  , [w | PDrop d w <- ps, not (d && w == "style")]
  )

categories :: P ([Cat], [Text])
categories = do
  cs <- between (symbol "{") (symbol "}") (cat `sepBy` symbol ",")
  order <- many (choice (map (\k -> k <$ kw k) ["rot", "ran", "randomize", "rev", "asc", "desc", "fix", "namespace"]))
  pure (cs, order)
  where
    cat = do
      n <- name <|> ("-" <$ symbol "-")
      ps <- labelProps
      ks <- many (keyword <* optional (balanced "(" ")"))
      let (ls, _, _) = collect ps
      pure (Cat n ls ks)
    keyword = try $ do
      k <- name
      notFollowedBy (symbol ":")
      pure (T.toLower k)

field :: P Field
field = do
  n <- name
  ps <- labelProps
  let (ls, dd, dropped) = collect ps
  t <-
    choice
      [ kw "categorical" *> ((\r (cs, _) -> FCat r cs) <$> option (Nothing, Nothing) range <*> option ([], []) categories)
      , kw "text" *> (FText <$> option (Nothing, Nothing) range)
      , kw "long" *> (FLong <$> option (Nothing, Nothing) range)
      , kw "double" *> (FDouble <$> option (Nothing, Nothing) range)
      , kw "date" *> (FDate <$ optional range)
      , kw "boolean" *> pure FBool
      , kw "info" *> pure FInfo
      , kw "loop" *> loop
      , kw "page" *> (FPage <$> between (symbol "(") (symbol ")") (name `sepBy` symbol ","))
      , kw "block" *> kw "fields" *> (FBlock <$> fieldList)
      , kw "lcl" *> (FArea . T.strip . T.takeWhile (/= ',') <$> balanced "(" ")")
      , pure FInfo
      ]
  rest <- T.strip . T.pack <$> manyTill anySingle (symbol ";")
  pure (Field n ls dd (dropped ++ [rest | not (T.null rest)]) t)
  where
    loop = do
      it <- (Left <$> range) <|> (Right . fst <$> categories)
      kw "fields"
      _ <- optional str
      fs <- fieldList
      _ <- many (choice (map kw ["row", "column", "expand", "grid"]))
      pure (FLoop it fs)

fieldList :: P [Field]
fieldList = symbol "(" *> manyTill (try field <|> rawField) (symbol ")")

-- | An entry the parser does not model, kept verbatim up to its @;@.
rawField :: P Field
rawField = do
  (src, _) <- match (someTill (balanced "{" "}" <|> balanced "(" ")" <|> str <|> T.singleton <$> anySingle) (symbol ";"))
  let t = T.strip src
  pure (Field (T.takeWhile isNameChar t) [] False [] (FRaw t))

metadata :: P ([Field], Text)
metadata = do
  sc
  kw "Metadata"
  params <- balanced "(" ")"
  fs <- manyTill (try field <|> rawField) (try (kw "End" *> kw "Metadata"))
  pure (fs, T.strip (T.takeWhile (/= ',') params))

-- | The routing section's lines, if there is one.
routingLines :: Text -> Maybe [Text]
routingLines src =
  let ls = map (T.strip . stripComment) (T.lines src)
   in case dropWhile (not . T.isPrefixOf "routing(" . T.toLower) ls of
        _ : body -> Just (filter (not . T.null) (takeWhile ((/= "end routing") . T.toLower . T.unwords . T.words) body))
        [] -> Nothing
  where
    -- A ' outside a string starts a comment.
    stripComment l = T.take (commentAt 0 False (T.unpack l)) l
    commentAt :: Int -> Bool -> String -> Int
    commentAt n _ [] = n
    commentAt n inStr (c : cs)
      | c == '"' = commentAt (n + 1) (not inStr) cs
      | c == '\'' && not inStr = n
      | otherwise = commentAt (n + 1) inStr cs

------------------------------------------------------------------------------
-- Reading

readMrScript :: BL.ByteString -> Conv Form
readMrScript bs = do
  let src = TE.decodeUtf8Lenient (BL.toStrict bs)
  (fields, defLang) <- either (Left . errorBundlePretty) Right (parse metadata "mrScript" src) `orFail` "mrScript metadata"
  let areas = M.fromList [(T.toLower (fName f), l) | f <- fields, FArea l <- [fType f]]
      rsection = routingLines src
      rlines = fromMaybe [] rsection
  items0 <- concat <$> mapM (fieldItems areas) [f | f <- fields, not (isArea f), not (isPage f)]
  routing <- readRouting rlines
  let pages = [(f, ns) | f <- fields, FPage ns <- [fType f]]
      grouped = fst (foldl (addPage areas) (items0, Nothing) pages)
      items = map (applyRouting (isJust rsection) routing) grouped
      askedPages = [m | (f, ms) <- pages, M.member (fName f) (rAsked routing), m <- ms]
  forM_ [q | q <- M.keys (rAsked routing), q `notElem` map itemId (allItems items)] $ \q -> warn q "routing asks a field that is not defined"
  let areaLangs = [l | f <- fields, FArea l <- [fType f]]
      meta = emptyMeta {metaLanguages = if null areaLangs then [defLang | defLang /= "en"] else nub (defLang : areaLangs)}
      s = Form meta items
      idx = choiceIndex False s
  when (isJust rsection) $
    forM_ [q | q <- questions s, qId q `M.notMember` rAsked routing, qId q `notElem` askedPages, not (isCalcOrHidden q)] $ \q -> warn (qId q) "never asked in the routing"
  pure (mapSurveyLogic (fromSelected idx (\_ v -> v)) s)
  where
    isArea f = case fType f of FArea _ -> True; _ -> False
    isPage f = case fType f of FPage _ -> True; _ -> False
    isCalcOrHidden q = case qType q of QCalculate _ -> True; QHidden -> True; _ -> False

i18nOf :: M.Map Text Lang -> [(Maybe Text, Text)] -> Maybe I18n
i18nOf areas ls = case [t | (Nothing, t) <- ls] of
  (d : _) -> Just (I18n d (M.fromList [(l, t) | (Just a, t) <- ls, Just l <- [M.lookup (T.toLower a) areas]]))
  [] -> Nothing

fieldItems :: M.Map Text Lang -> Field -> Conv [Item]
fieldItems areas f = do
  forM_ (fDropped f) $ \d -> warn (fName f) (d <> " dropped")
  let label = fromMaybe (plain "") (i18nOf areas (fLabels f))
      q t = ItemQuestion (question (fName f) "" t) {qLabel = label, qRequired = True}
      withRange t (lo, hi) = case q t of
        ItemQuestion x | isJust lo || isJust hi -> ItemQuestion x {qValidation = Just (Validation lo hi Nothing Nothing)}
        i -> i
  case fType f of
    FCat (_, hi) cs -> do
      chs <- cats cs
      let sel = if hi == Just 1 then Single else Multiple
      pure [q (QChoice sel (if fDropdown f then Dropdown else Radio) chs)]
    FText (_, hi) -> pure [q (QText (if maybe False (>= 1000) hi then LongText else ShortText))]
    FLong r -> pure [withRange (QNumber Integer) r]
    FDouble r -> pure [withRange (QNumber Decimal) r]
    FDate -> pure [q (QDate Date)]
    FBool -> pure [q QBoolean]
    FInfo -> pure [ItemNote (fName f) label Nothing]
    FLoop (Right rows) [inner] | FCat (_, hi) cols <- fType inner -> do
      rs <- cats rows
      cs <- cats cols
      pure
        [ case q (QMatrix (if hi == Just 1 then Single else Multiple) rs cs) of
            ItemQuestion x -> ItemQuestion x {qAttrs = M.singleton "mrscript:field" (String (fName inner))}
            i -> i
        ]
    FLoop _ inner -> do
      warn (fName f) "loop read as a repeat group; its iterations are dropped"
      kids <- concat <$> mapM (fieldItems areas) inner
      pure [ItemGroup (Group (fName f) Repeat (i18nOf areas (fLabels f)) Nothing kids M.empty)]
    FBlock inner -> do
      kids <- concat <$> mapM (fieldItems areas) inner
      pure [ItemGroup (Group (fName f) Section (i18nOf areas (fLabels f)) Nothing kids M.empty)]
    FRaw t -> pure [ItemRaw (Format "mrscript") (String t)]
    FPage _ -> pure []
    FArea _ -> pure []
  where
    cats cs = forM (zip [1 :: Int ..] cs) $ \(n, c) -> do
      let ks = catKeywords c
          v
            | catName c == "-" = if "other" `elem` ks then "other" else "c" <> T.pack (show n)
            | otherwise = codeValue (catName c)
          l = fromMaybe (plain (catName c)) (i18nOf areas (catLabels c))
      forM_ [k | k <- ks, k `notElem` ["other", "exclusive", "fix"]] $ \k -> warn (fName f) ("category keyword " <> k <> " on " <> catName c <> " dropped")
      pure (Choice v l ("other" `elem` ks) ("exclusive" `elem` ks))

-- | @_1@ is how a numeric category value is named.
codeValue :: Text -> Text
codeValue n = case T.uncons n of
  Just ('_', rest) | Just (c, _) <- T.uncons rest, isDigit c -> rest
  _ -> n

-- | Gather a page's members where the first of them was. A page with no
-- members goes after the page declared before it.
addPage :: M.Map Text Lang -> ([Item], Maybe Text) -> (Field, [Text]) -> ([Item], Maybe Text)
addPage areas (items, prev) (f, members) =
  let memberSet = S.fromList members
      isMember i = S.member (itemId i) memberSet
      byId = M.fromListWith (\_ a -> a) [(itemId i, i) | i <- items, isMember i]
      inPage = [i | m <- members, Just i <- [M.lookup m byId]]
      g = ItemGroup (Group (fName f) Page (i18nOf areas (fLabels f)) Nothing inPage M.empty)
      afterPrev = case prev of
        Just p | (before, x : after) <- break ((== p) . itemId) items -> before ++ [x, g] ++ after
        _ -> g : items
   in ( case break isMember items of
          (before, _ : after) -> before ++ [g] ++ filter (not . isMember) after
          (_, []) -> afterPrev
      , Just (fName f)
      )

data Routing = Routing
  { rAsked :: M.Map Text (Maybe Expr)
  , rOptional :: S.Set Text
  , rCalc :: M.Map Text Expr
  }

-- | Routing statements, with the conditions of their enclosing @If@s.
readRouting :: [Text] -> Conv Routing
readRouting = go [] (Routing M.empty S.empty M.empty)
  where
    -- The stack holds, per open If, the tests of the branches tried so far
    -- (the last is the current branch's own; an Else has none).
    go _ r [] = pure r
    go stack r (l : ls) =
      let low = T.toLower l
          ctx = foldr (andMaybe . Just) Nothing (reverse (map branch stack))
       in case () of
            _
              | Just c <- ifThen l, Just stmt <- nonEmptyT (snd c) -> do
                  r' <- statement (andMaybe ctx (Just (fst c))) r stmt
                  go stack r' ls
              | Just (c, _) <- ifThen l -> go (stack ++ [([], Just c)]) r ls
              | "elseif " `T.isPrefixOf` low, Just (c, _) <- ifThen (T.drop 4 l) ->
                  go (init' stack ++ [(tried stack, Just c)]) r ls
              | low == "else" -> go (init' stack ++ [(tried stack, Nothing)]) r ls
              | low `elem` ["end if", "endif"] -> go (init' stack) r ls
              | otherwise -> statement ctx r l >>= \r' -> go stack r' ls
    init' xs = take (length xs - 1) xs
    -- A branch runs when its own test holds and no earlier branch's did.
    branch (earlier, own) = case map ENot earlier ++ maybe [] pure own of
      [] -> ELit (LBool True)
      (x : xs) -> foldl (EBin And) x xs
    tried stack = case reverse stack of
      ((earlier, own) : _) -> earlier ++ maybe [] pure own
      [] -> [ELit (LBool True)]
    nonEmptyT t = if T.null (T.strip t) then Nothing else Just (T.strip t)

-- | @If cond Then rest@: the condition and whatever follows @Then@.
ifThen :: Text -> Maybe (Expr, Text)
ifThen l
  | "if " `T.isPrefixOf` T.toLower l =
      let body = T.drop 3 l
          (c, rest) = T.breakOnEnd " then" (T.toLower body)
       in if T.null c
            then Nothing
            else
              let n = T.length c - 5
               in Just (parseOrRaw mrscript (T.strip (T.take n body)), T.drop (T.length body - T.length rest) body)
  | otherwise = Nothing

statement :: Maybe Expr -> Routing -> Text -> Conv Routing
statement ctx r l
  | Just q <- method [".ask()", ".ask", ".show()", ".show"] = do
      when (M.member q (rAsked r)) $ warn q "asked more than once in the routing; read as one question, shown when any of those asks runs"
      pure r {rAsked = M.insertWith either' q ctx (rAsked r)}
  | (lhs, rhs) <- T.breakOn "=" l
  , Just q <- T.stripSuffix ".mustanswer" (T.toLower (T.strip lhs))
  , T.toLower (T.strip (T.drop 1 rhs)) == "false" =
      pure r {rOptional = S.insert (orig q) (rOptional r)}
  | (lhs, rhs) <- T.breakOn "=" l
  , Just q <- T.stripSuffix ".response.value" (T.toLower (T.strip lhs)) =
      do
        let value = parseOrRaw mrscript (T.strip (T.drop 1 rhs))
        when (maybe False (/= value) (M.lookup (orig q) (rCalc r))) $ warn (orig q) "assigned more than once in the routing; only the last value is kept"
        pure r {rCalc = M.insert (orig q) value (rCalc r), rAsked = M.insertWith either' (orig q) ctx (rAsked r)}
  | otherwise = r <$ warn "routing" ("statement ignored: " <> l)
  where
    -- Asked on either path; an ask outside every If always runs.
    either' new old = EBin Or <$> old <*> new
    method ms = listToMaybe [T.strip (T.take (T.length l - T.length m) l) | m <- ms, m `T.isSuffixOf` T.toLower l, T.all isNameChar (T.strip (T.take (T.length l - T.length m) l))]
    orig q = T.take (T.length q) (T.strip l)

applyRouting :: Bool -> Routing -> Item -> Item
applyRouting hasRouting r = go False
  where
    condOf i = join (M.lookup i (rAsked r))
    go viaPage = \case
      ItemGroup g ->
        let pageAsked = groupKind g == Page && M.member (groupId g) (rAsked r)
         in ItemGroup
              g
                { groupRelevant = if pageAsked then condOf (groupId g) else groupRelevant g
                , groupItems = map (go (viaPage || pageAsked)) (groupItems g)
                }
      ItemNote i t _ -> ItemNote i t (condOf i)
      ItemQuestion q ->
        let asked = viaPage || M.member (qId q) (rAsked r)
            t = case (M.lookup (qId q) (rCalc r), qType q) of
              (Just e, _) -> QCalculate e
              (Nothing, QText _) | hasRouting, not asked -> QHidden
              (_, other) -> other
         in ItemQuestion
              q
                { qRelevant = case t of
                    QHidden -> Nothing
                    _ -> condOf (qId q)
                , qRequired = not hasRouting || (not (isData t) && qId q `S.notMember` rOptional r)
                , qType = t
                }
      i -> i
    isData = \case
      QHidden -> True
      QCalculate _ -> True
      _ -> False

------------------------------------------------------------------------------
-- Writing

writeMrScript :: Form -> Conv BL.ByteString
writeMrScript s0@(Form meta _) = do
  s <- legalIds "item" mrName s0
  when (isJust (metaTitle meta)) $ warn "mrscript" "survey title dropped"
  when (isJust (metaDescription meta)) $ warn "mrscript" "survey description dropped"
  let langs = surveyLangs s
      defLang = fromMaybe "en" (listToMaybe (metaLanguages meta))
      others = filter (/= defLang) (nub (drop 1 (metaLanguages meta) ++ langs))
      idx = choiceIndex False s
  s'@(Form _ items) <- dropUnprintable mrscript (mapSurveyLogic (toSelected idx (\_ v -> catCode v)) s)
  when (not (null langs) && null (metaLanguages meta)) $ warn "mrscript" "default language assumed to be en"
  fields <- concat <$> mapM (writeItem others) items
  let pageFields = [pageField others g | ItemGroup g <- items, groupKind g == Page]
      areaFields = ["    " <> areaName l <> " lcl(" <> l <> ", Question, Label);" | l <- others]
  routing <- concat <$> mapM routeItem (formItems s')
  let out =
        ["Metadata(" <> defLang <> ", Question, Label)"]
          ++ areaFields
          ++ fields
          ++ pageFields
          ++ ["End Metadata", "", "Routing(Web)"]
          ++ map ("    " <>) routing
          ++ ["End Routing"]
  pure (BL.fromStrict (TE.encodeUtf8 (T.unlines out)))

areaName :: Lang -> Text
areaName = T.toUpper . T.map (\c -> if isAlphaNum c then c else '_')

-- | A valid mrScript name is kept, anything else sanitised; applying it
-- twice changes nothing.
mrName :: Text -> Text
mrName v
  | validName clean = clean
  | otherwise = "_" <> clean
  where
    clean = T.map (\c -> if isAlphaNum c || c == '_' then c else '_') v

validName :: Text -> Bool
validName v = case T.uncons v of
  Just (c, rest) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x == '_') rest
  Nothing -> False

catCode :: Text -> Text
catCode v
  | validName v = v
  | otherwise = "_" <> T.map (\c -> if isAlphaNum c || c == '_' then c else '_') v

quote :: Text -> Text
quote t = "\"" <> T.replace "\"" "\"\"" t <> "\""

labelText :: [Lang] -> I18n -> Text
labelText others (I18n d tr) = T.unwords (quote d : [areaName l <> ": " <> quote t | l <- others, Just t <- [M.lookup l tr]])

pageField :: [Lang] -> Group -> Text
pageField others g =
  "    " <> groupId g <> maybe "" ((" " <>) . labelText others) (groupTitle g) <> " page(" <> T.intercalate ", " [itemId i | i <- flattenGroups (groupItems g), isField i] <> ");"
  where
    isField = \case
      ItemQuestion _ -> True
      ItemNote {} -> True
      _ -> False

writeItem :: [Lang] -> Item -> Conv [Text]
writeItem others = \case
  ItemRaw (Format "mrscript") (String t) -> pure ["    " <> t]
  ItemRaw (Format f) _ -> [] <$ warn "mrscript" ("raw " <> f <> " content dropped")
  ItemNote i t _ -> pure ["    " <> i <> " " <> labelText others t <> " info;", ""]
  ItemGroup g -> do
    case groupKind g of
      Page -> pure ()
      Section -> when (isJust (groupTitle g)) $ warn (groupId g) "section title dropped"
      Repeat -> warn (groupId g) "repeat group flattened"
    concat <$> mapM (writeItem others) (groupItems g)
  ItemQuestion q -> writeQuestion others q

writeQuestion :: [Lang] -> Question -> Conv [Text]
writeQuestion others q = do
  let note = warn (qId q)
      hd = "    " <> qId q <> " " <> labelText others (qLabel q)
      rng v = case v of
        Just (Validation lo hi _ _) | isJust lo || isJust hi -> " [" <> maybe "" num lo <> ".." <> maybe "" num hi <> "]"
        _ -> ""
      simple t = pure [hd <> " " <> t <> ";", ""]
  when (isJust (qHint q)) $ note "hint dropped"
  when (isJust (qDefault q)) $ note "default value dropped"
  forM_ (qValidation q) $ \v -> do
    when (isJust (valExpr v)) $ note "validation expression dropped"
    when (isJust (valMessage v)) $ note "validation message dropped"
  case qType q of
    QChoice sel style cs -> do
      when (style == Buttons) $ note "button style dropped"
      ls <- catLines cs
      let st = if style == Dropdown then " style(Control(Type = \"DropList\"))" else ""
      pure ([hd <> st <> " categorical " <> (if sel == Single then "[1..1]" else "[1..]")] ++ ls ++ [""])
    QRanking cs -> do
      note "ranking written as a multi-select"
      ls <- catLines cs
      pure ([hd <> " categorical [1..]"] ++ ls ++ [""])
    QMatrix sel rs cs -> do
      rows <- catLines rs
      cols <- catLines cs
      let inner = case M.lookup "mrscript:field" (qAttrs q) of
            Just (String n) -> n
            _ -> "Column"
      pure $
        [hd <> " loop"]
          ++ map T.stripEnd (init' rows)
          ++ ["    } fields", "    ("]
          ++ ["        " <> inner <> " categorical " <> (if sel == Single then "[1..1]" else "[1..]")]
          ++ map ("    " <>) (init' cols)
          ++ ["        };", "    ) expand grid;", ""]
    QScale lo hi l h -> do
      note (if isJust l || isJust h then "scale written as a whole number; end labels dropped" else "scale written as a whole number")
      simple ("long [" <> T.pack (show lo) <> ".." <> T.pack (show hi) <> "]")
    QText k -> do
      when (k `elem` [Email, Url, Phone]) $ note "input kind written as plain text"
      simple (if k == LongText then "text [..4000]" else "text")
    QNumber Integer -> simple ("long" <> rng (qValidation q))
    QNumber Decimal -> simple ("double" <> rng (qValidation q))
    QDate k -> do
      when (k /= Date) $ note "time written as a date"
      simple "date"
    QBoolean -> simple "boolean"
    QFile -> note "file upload written as text" >> simple "text"
    QGeo -> note "geopoint written as text" >> simple "text"
    QHidden -> simple "text"
    QCalculate _ -> simple "text"
    QOther t -> note ("type " <> t <> " written as text") >> simple "text"
  where
    num x = if x == fromIntegral (round x :: Integer) then T.pack (show (round x :: Integer)) else T.pack (show x)
    init' xs = take (length xs - 1) xs
    catLines cs = do
      body <- forM (zip [1 :: Int ..] cs) $ \(n, c) -> do
        let code = catCode (choiceValue c)
        unless (code == choiceValue c || codeValue code == choiceValue c) $ warn (qId q) ("choice value " <> choiceValue c <> " written as " <> code)
        let kws = [" other" | choiceOther c] ++ [" exclusive" | choiceExclusive c]
        pure ("        " <> code <> " " <> labelText others (choiceLabel c) <> T.concat kws <> (if n == length cs then "" else ","))
      pure (["    {"] ++ body ++ ["    };"])

routeItem :: Item -> Conv [Text]
routeItem = \case
  ItemNote i _ rel -> ask i ".Show()" rel
  ItemRaw _ _ -> pure []
  ItemGroup g
    | groupKind g == Page, members <- flattenGroups (groupItems g), all ownless members, all isAsked members -> do
        a <- ask (groupId g) ".Ask()" (groupRelevant g)
        pure ([qId q <> ".MustAnswer = False" | ItemQuestion q <- members, not (qRequired q)] ++ a)
    | otherwise -> do
        when (isJust (groupRelevant g)) $ warn (groupId g) "page condition moved onto its questions"
        concat <$> mapM routeItem (flattenGroups [ItemGroup g])
  ItemQuestion q -> case qType q of
    QHidden -> pure []
    QCalculate e -> do
      rhs <- printLogic mrscript (qId q) (Just e)
      case rhs of
        Just t -> guarded (qId q) (qRelevant q) (qId q <> ".Response.Value = " <> t)
        Nothing -> pure []
    _ -> do
      a <- ask (qId q) ".Ask()" (qRelevant q)
      pure ([qId q <> ".MustAnswer = False" | not (qRequired q)] ++ a)
  where
    ask i m rel = guarded i rel (i <> m)
    guarded i rel stmt = do
      c <- printLogic mrscript i rel
      pure [maybe stmt (\t -> "If " <> t <> " Then " <> stmt) c]
    ownless = \case
      ItemQuestion q -> isNothing (qRelevant q)
      ItemNote _ _ r -> isNothing r
      ItemRaw _ _ -> True
      _ -> False
    isAsked = \case
      ItemQuestion q -> case qType q of QHidden -> False; QCalculate _ -> False; _ -> True
      _ -> True

