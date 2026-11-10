-- | XLSForm (ODK, KoboToolbox, SurveyCTO): a @survey@ sheet of rows, a
-- @choices@ sheet of option lists and a @settings@ sheet. Read and written
-- both as .xlsx and as pyxform's single-file CSV, where a row whose first
-- cell names a sheet starts that sheet's header.
module Panform.Format.XLSForm
  ( readXLSForm
  , writeXLSForm
  , readXLSFormCsv
  , writeXLSFormCsv
  , fromSheets
  , toSheets
  ) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, lift, put)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Aeson (Result (..), Value (String), fromJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Csv as C
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Csv (parseCells)
import Panform.Xlsx

type Row = M.Map Text Text

cell :: Text -> Row -> Text
cell k = M.findWithDefault "" k

------------------------------------------------------------------------------
-- Containers

readXLSForm :: BL.ByteString -> Conv Form
readXLSForm bs = readXlsx bs `orFail` "XLSForm .xlsx" >>= fromSheets

writeXLSForm :: Form -> Conv BL.ByteString
writeXLSForm s = writeXlsx <$> toSheets s

readXLSFormCsv :: BL.ByteString -> Conv Form
readXLSFormCsv bs = do
  let rows = parseCells ',' bs
  fromSheets (split (rows :: [[Text]]))
  where
    -- A sheet's header is on the sheet-name row, or on the next row when
    -- the sheet name stands alone (pyxform's own CSV test files).
    split = reverse . map (fmap (dropBlankHeader . reverse)) . foldl step []
    dropBlankHeader rs = case rs of
      (h : more) | all (T.null . T.strip) h -> more
      _ -> rs
    step acc (name : rest) | not (T.null (T.strip name)) = (T.strip name, [rest]) : acc
    step ((n, rs) : acc) (_ : rest) = (n, rest : rs) : acc
    step acc _ = acc

writeXLSFormCsv :: Form -> Conv BL.ByteString
writeXLSFormCsv s = do
  sheets <- toSheets s
  let rows = concat [(name : hdr) : map ("" :) body | (name, hdr : body) <- sheets]
      width = maximum (1 : map length rows)
      pad r = r ++ replicate (width - length r) ""
  pure (C.encode (map (map TE.encodeUtf8 . pad) rows))

------------------------------------------------------------------------------
-- Reading

-- | Header-keyed rows. Headers are lower-cased and trimmed except for the
-- language suffix, and @label:English@ spellings become @label::English@.
sheetRows :: Text -> [Sheet] -> [Row]
sheetRows name sheets = case [rs | (n, rs) <- sheets, T.toLower (T.strip n) == name] of
  ((hdr : body) : _) ->
    let keys = map normHeader hdr
     in [M.fromList (filter (not . T.null . fst) (zip keys (zipWith clean keys (r ++ repeat "")))) | r <- body, any (not . T.all isSpace) r]
  _ -> []
  where
    -- Text a respondent reads is kept exactly; structural cells are trimmed.
    clean k v = if T.takeWhile (/= ':') k `elem` ["label", "hint", "constraint_message", "required_message", "media", "form_title"] then v else T.strip v

normHeader :: Text -> Text
normHeader h = case T.breakOn "::" (T.strip h) of
  (base, rest) | not (T.null rest) -> T.toLower (T.strip base) <> "::" <> T.strip (T.drop 2 rest)
  _ -> case T.breakOn ":" (T.strip h) of
    (base, rest) | T.toLower base `elem` ["label", "hint", "constraint_message", "required_message", "image", "audio", "media::image"], not (T.null rest) ->
      T.toLower base <> "::" <> T.strip (T.drop 1 rest)
    _ -> T.replace " " "_" (T.toLower (T.strip h))

-- | "English (en)" -> "en"; anything else is used as the tag itself.
langCode :: Text -> Lang
langCode l = case T.breakOnEnd "(" l of
  (pre, code) | not (T.null pre), ")" `T.isSuffixOf` code -> T.dropEnd 1 code
  _ -> l

data Langs = Langs
  { lDefault :: Maybe Text
  -- ^ Header suffix of the default language, when there is no plain column.
  }

i18nFrom :: Langs -> Text -> Row -> Maybe I18n
i18nFrom langs base row =
  let plainCol = M.lookup base row
      trans = [(suffix, t) | (k, t) <- M.toList row, Just suffix <- [T.stripPrefix (base <> "::") k], not (T.null t)]
      def = case plainCol of
        Just t | not (T.null t) -> Just t
        _ -> (lDefault langs >>= (`lookup` trans)) `orElse` fmap snd (listToMaybe trans)
      defSuffix = case plainCol of
        Just t | not (T.null t) -> Nothing
        _ -> if isJust (lDefault langs >>= (`lookup` trans)) then lDefault langs else fst <$> listToMaybe trans
   in (\d -> I18n d (M.fromList [(langCode s, t) | (s, t) <- trans, Just s /= defSuffix])) <$> def
  where
    orElse a b = maybe b Just a

fromSheets :: [Sheet] -> Conv Form
fromSheets sheets = do
  let survey = sheetRows "survey" sheets
      choicesRows = sheetRows "choices" sheets
      settings = fromMaybe M.empty (listToMaybe (sheetRows "settings" sheets))
      -- Only translatable columns name a language: @media::image@ and
      -- @bind::...@ are not translations.
      suffixes = nub [s | r <- survey ++ choicesRows, k <- M.keys r, Just (b, s) <- [splitLang k], b `elem` translatable]
      defLang = case cell "default_language" settings of
        "" -> Nothing
        d -> Just (fromMaybe d (listToMaybe [s | s <- suffixes, s == d || langCode s == d]))
      langs = Langs defLang
      -- Built newest-first and reversed once: appending one choice at a
      -- time is quadratic in the length of a list.
      lists =
        M.map reverse $
          M.fromListWith
            (++)
            [ (cell "list_name" r, [Choice (cell "name" r) (fromMaybe (plain (if any ("label" `T.isPrefixOf`) (M.keys r) then "" else cell "name" r)) (i18nFrom langs "label" r)) False False])
            | r <- choicesRows
            , not (T.null (cell "list_name" r))
            ]
      -- Columns of the choices sheet the IR has no field for (cascade
      -- filter columns, media), by list and choice name. Their headers are
      -- kept as written: a choice_filter names them exactly.
      choiceExtras0 = case [rs | (n, rs) <- sheets, T.toLower (T.strip n) == "choices"] of
        ((hdr : body) : _) ->
          M.fromListWith
            (\new old -> M.union old new)
            [ (l, M.singleton (at "name") extra)
            | r <- body
            , let cells = zip (map T.strip hdr) (map T.strip (r ++ repeat ""))
                  at k = fromMaybe "" (listToMaybe [v | (h, v) <- cells, normHeader h == k])
                  l = at "list_name"
                  extra = M.fromList [(h, v) | (h, v) <- cells, not (T.null h), not (T.null v), T.takeWhile (/= ':') (normHeader h) `notElem` ["list_name", "name", "label"]]
            , not (T.null l)
            , not (M.null extra)
            ]
        _ -> M.empty
  unless (any ((== "survey") . T.toLower . T.strip . fst) sheets) $ failWith "XLSForm has no survey sheet"
  items <- evalStateT (parseRows langs lists Nothing) survey
  -- A list no question uses has no place in the IR.
  let usedLists = S.fromList [l | ItemQuestion q <- allItems items, Just (String l) <- [M.lookup "xlsform:list" (qAttrs q)]]
      unused = [l | l <- M.keys lists, not (S.member l usedLists)]
  unless (null unused) $ warn "xlsform" ("choice lists no question uses dropped: " <> T.intercalate ", " unused)
  let choiceExtras = M.filterWithKey (\l _ -> S.member l usedLists) choiceExtras0
      allLangs = case defLang of
        Just d -> nub (langCode d : map langCode suffixes)
        Nothing -> []
      meta =
        emptyMeta
          { metaId = nonEmpty (cell "form_id" settings)
          , metaTitle = plain <$> nonEmpty (cell "form_title" settings)
          , metaLanguages = allLangs
          , metaAttrs =
              M.fromList $
                [("xlsform:" <> k, String v) | (k, v) <- M.toList settings, k `notElem` ["form_id", "form_title", "default_language"], not (T.null v)]
                  ++ [(choiceColumnsKey, toJSON choiceExtras) | not (M.null choiceExtras)]
          }
  pure (Form meta items)
  where
    splitLang k = case T.breakOn "::" k of
      (b, rest) | not (T.null rest) -> Just (b, T.drop 2 rest)
      _ -> Nothing

-- | Columns whose @::suffix@ is a language.
translatable :: [Text]
translatable = ["label", "hint", "constraint_message", "required_message", "guidance_hint"]

-- | Where the extra columns of the choices sheet are kept. The key has no
-- settings-sheet spelling, so it cannot collide with a setting.
choiceColumnsKey :: Text
choiceColumnsKey = "xlsform:choices sheet columns"

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

-- | Consumes rows until the @end@ marker that closes the open group (its
-- name and kind are given), or the end of the sheet. A marker that closes
-- nothing is skipped and one of the wrong kind still closes, each with a
-- warning, so no row after it goes unread.
parseRows :: Langs -> M.Map Text [Choice] -> Maybe (Text, Text) -> StateT [Row] Conv [Item]
parseRows langs lists open = go []
  where
    go acc = do
      rows <- get
      case rows of
        [] -> do
          mapM_ (\(n, k) -> lift (warn n ("begin " <> k <> " is never closed; closed at the end of the sheet"))) open
          pure (reverse acc)
        (r : rest) -> do
          put rest
          let ws = T.words (T.replace "_" " " (T.toLower (cell "type" r)))
          case ws of
            ["end", k] -> case open of
              Nothing -> lift (warn "xlsform" ("end " <> k <> " without a matching begin ignored")) >> go acc
              Just (n, k') -> do
                when (k /= k') $ lift (warn n ("begin " <> k' <> " closed by end " <> k))
                pure (reverse acc)
            ["begin", kind] -> do
              kids <- parseRows langs lists (Just (cell "name" r, kind))
              let appearance = cell "appearance" r
                  gk
                    | kind == "repeat" = Repeat
                    | "field-list" `T.isInfixOf` appearance = Page
                    | otherwise = Section
                  g =
                    Group
                      { groupId = cell "name" r
                      , groupKind = gk
                      , groupTitle = i18nFrom langs "label" r
                      , groupRelevant = parseOrRaw xlsform <$> nonEmpty (cell "relevant" r)
                      , groupItems = kids
                      , groupAttrs = extraAttrs r
                      }
                  lost = [k | (k, v) <- M.toList r, not (T.null v), T.takeWhile (/= ':') k `elem` ["hint", "required", "constraint", "constraint_message", "calculation", "default"]]
              unless (null lost) $ lift (warn (groupId g) ("group columns dropped: " <> T.intercalate ", " lost))
              go (asMatrix g : acc)
            []
              | M.null (M.filter (not . T.null) r) -> go acc
              | otherwise -> lift (warn (cell "name" r) "row without a type skipped") >> go acc
            _ -> do
              item <- lift (rowItem langs lists r)
              go (item : acc)

-- | The ODK matrix idiom, as the writer emits it: a field-list group of
-- selects named @<group>_<row>@ that share one choice list, each with
-- @list-nolabel@ appearance and nothing else of its own. A group whose rows
-- carry logic, a hint, a default, validation, other columns or differing
-- required flags stays a group, because a matrix has no place for them.
asMatrix :: Group -> Item
asMatrix g = case mapM row (groupItems g) of
  Just rs@((sel, cols, q0) : _)
    | groupKind g == Page
    , M.keys (groupAttrs g) == ["xlsform:appearance"]
    , M.lookup "xlsform:appearance" (groupAttrs g) == Just (String "field-list")
    , all (\(s, c, q) -> s == sel && c == cols && qRequired q == qRequired q0) rs
    , not (any choiceOther cols) ->
        ItemQuestion
          (question (groupId g) "" (QMatrix sel [Choice v (qLabel q) False False | (_, _, q) <- rs, Just v <- [T.stripPrefix prefix (qId q)]] cols))
            { qLabel = fromMaybe (plain "") (groupTitle g)
            , qAttrs = maybe M.empty (M.singleton "xlsform:list") (M.lookup "xlsform:list" (qAttrs q0))
            , qRelevant = groupRelevant g
            , qRequired = qRequired q0
            }
  _ -> ItemGroup g
  where
    prefix = groupId g <> "_"
    row (ItemQuestion q)
      | M.lookup "xlsform:appearance" (qAttrs q) == Just (String "list-nolabel")
      , all (`elem` ["xlsform:appearance", "xlsform:list"]) (M.keys (qAttrs q))
      , QChoice sel _ cs <- qType q
      , prefix `T.isPrefixOf` qId q
      , isNothing (qRelevant q) && isNothing (qHint q) && isNothing (qValidation q) && isNothing (qDefault q) =
          Just (sel, cs, q)
    row _ = Nothing

-- | What a simple bound constrains: the length of a text answer, the
-- value of any other.
boundSubject :: QType -> Expr
boundSubject = \case
  QText _ -> ECall "string-length" [ERef "."]
  _ -> ERef "."

-- | The bounds as conjuncts, minimum first.
boundTerms :: QType -> Validation -> [Expr]
boundTerms t v =
  [EBin Ge (boundSubject t) (ELit (LNum x)) | Just x <- [valMin v]]
    ++ [EBin Le (boundSubject t) (ELit (LNum x)) | Just x <- [valMax v]]

-- | A constraint that opens with @. >= a@ and/or @. <= b@ (or the same on
-- @string-length(.)@ for a text question) carries those as bounds; the
-- rest of the conjunction stays an expression. The inverse of writing
-- 'boundTerms' in front of the expression.
splitBounds :: QType -> Expr -> (Maybe Double, Maybe Double, Maybe Expr)
splitBounds t e = go Nothing Nothing (spine e)
  where
    subject = boundSubject t
    spine = \case
      EBin And a b -> spine a ++ [b]
      x -> [x]
    go Nothing hi (EBin Ge s (ELit (LNum n)) : r) | s == subject = go (Just n) hi r
    go lo Nothing (EBin Le s (ELit (LNum n)) : r) | s == subject = go lo (Just n) r
    go lo hi r = (lo, hi, case r of [] -> Nothing; (x : xs) -> Just (foldl (EBin And) x xs))

-- | Columns the IR has no field for, kept so an XLSForm round trip keeps them.
extraAttrs :: Row -> Attrs
extraAttrs r =
  M.fromList
    [ ("xlsform:" <> k, String v)
    | (k, v) <- M.toList r
    , not (T.null v)
    , T.takeWhile (/= ':') k `notElem` known
    ]
  where
    known = ["type", "name", "label", "hint", "required", "relevant", "relevance", "constraint", "constraint_message", "calculation", "default"]

rowItem :: Langs -> M.Map Text [Choice] -> Row -> Conv Item
rowItem langs lists r = do
  let rawType = T.strip (cell "type" r)
      (ty, args) = case T.words rawType of
        (t : as) -> (T.toLower t, as)
        [] -> ("", [])
      name = cell "name" r
      label = fromMaybe (plain "") (i18nFrom langs "label" r)
      appearance = cell "appearance" r
      relevant = parseOrRaw xlsform <$> nonEmpty (cell "relevant" r `orT` cell "relevance" r)
      listOf = case args of
        (l : _) -> case M.lookup l lists of
          Just cs -> pure cs
          Nothing -> [] <$ warn name ("choice list " <> l <> " not found")
        [] -> [] <$ warn name "select without a choice list"
      withOther cs = if "or_other" `elem` args then cs ++ [Choice "other" (plain "Other") True False] else cs
      style
        | "minimal" `T.isInfixOf` appearance || "autocomplete" `T.isInfixOf` appearance = Dropdown
        | "likert" `T.isInfixOf` appearance || "quick" `T.isInfixOf` appearance = Buttons
        | otherwise = Radio
      (rangeLo, rangeHi) = rangeEnds (cell "parameters" r)
      -- The choice list's name, so a rewrite can reuse it.
      fileAttr = if ty `elem` ["image", "audio", "video", "background-audio"] then M.insert "xlsform:filetype" (String ty) else id
      listAttr = case args of
        (l : _) | ty `elem` ["select_one", "select1", "select_multiple", "rank"] -> M.insert "xlsform:list" (String l)
        _ -> id
  qt <- case ty of
    "text" -> pure (QText (if "multiline" `T.isInfixOf` appearance then LongText else ShortText))
    "string" -> pure (QText ShortText)
    "integer" -> pure (QNumber Integer)
    "int" -> pure (QNumber Integer)
    "decimal" -> pure (QNumber Decimal)
    "select_one" -> QChoice Single style . withOther <$> listOf
    "select1" -> QChoice Single style . withOther <$> listOf
    "select_multiple" -> QChoice Multiple style . withOther <$> listOf
    "rank" -> QRanking <$> listOf
    "date" -> pure (QDate Date)
    "time" -> pure (QDate Time)
    "datetime" -> pure (QDate DateTime)
    "geopoint" -> pure QGeo
    "geotrace" -> pure QGeo
    "geoshape" -> pure QGeo
    t | t `elem` ["image", "audio", "video", "file", "background-audio"] -> pure QFile
    "calculate" -> pure (QCalculate (parseOrRaw xlsform (cell "calculation" r)))
    "hidden" -> pure QHidden
    "acknowledge" -> pure QBoolean
    "range" -> pure (QScale rangeLo rangeHi Nothing Nothing)
    "note" -> pure (QOther "note")
    _ -> pure (QOther rawType)
  let message = i18nFrom langs "constraint_message" r
      validation = case nonEmpty (cell "constraint" r) of
        Just c -> let (lo, hi, rest) = splitBounds qt (parseOrRaw xlsform c) in Just (Validation lo hi rest message)
        Nothing -> (\m -> Validation Nothing Nothing Nothing (Just m)) <$> message
      requiredCell = T.toLower (cell "required" r)
      required = requiredCell `elem` ["yes", "true", "true()", "1", "y"]
      -- A required expression (@${age} >= 18@) is not a yes or a no: the
      -- question is not always required, and the expression is kept.
      requiredAttr =
        if required || requiredCell `elem` ["", "no", "false", "false()", "0", "n"]
          then id
          else M.insert "xlsform:required" (String (cell "required" r))
      noteLost = [k | (k, v) <- M.toList r, not (T.null v), T.takeWhile (/= ':') k `notElem` ["type", "name", "label", "relevant", "relevance"]]
  case qt of
    QOther "note" | not (null noteLost) -> warn name ("note columns dropped: " <> T.intercalate ", " noteLost)
    _ -> pure ()
  pure $ case qt of
    QOther "note" -> ItemNote name label relevant
    _ ->
      ItemQuestion
        Question
          { qId = name
          , qLabel = label
          , qHint = i18nFrom langs "hint" r
          , qType = qt
          , qRequired = required
          , qRelevant = relevant
          , qValidation = validation
          , qDefault = nonEmpty (cell "default" r)
          , qAttrs = requiredAttr (fileAttr (listAttr (extraAttrs r)))
          }
  where
    orT a b = if T.null a then b else a

-- | The ends of a @range@ from its @start=@ and @end=@ parameters.
rangeEnds :: Text -> (Int, Int)
rangeEnds cellText = (paramInt "start" 1, paramInt "end" 10)
  where
    params = M.fromList [(T.toLower k, T.drop 1 v) | p <- T.words cellText, let (k, v) = T.breakOn "=" p]
    paramInt k d = fromMaybe d (M.lookup k params >>= readNum)
    readNum t = case reads (T.unpack t) :: [(Double, String)] of
      [(d, "")] -> Just (round d)
      _ -> Nothing

------------------------------------------------------------------------------
-- Writing

toSheets :: Form -> Conv [Sheet]
toSheets (Form meta items0) = do
  -- Names are identifiers in XLSForm; surrounding spaces do not survive.
  let items = map trimIds items0
  when (items /= items0) $ warn "xlsform" "spaces around question or group names trimmed"
  when (isJust (metaDescription meta)) $ warn "xlsform" "survey description dropped (XLSForm has no description setting)"
  when (any (\c -> T.strip (choiceValue c) /= choiceValue c) (concatMap choicesOf (questions (Form meta items)))) $
    warn "xlsform" "spaces around choice names trimmed"
  when (maybe False (not . M.null . i18nTranslations) (metaTitle meta)) $ warn "xlsform" "title translations dropped"
  (rows, lists) <- evalStateT ((,) <$> fmap concat (mapM itemRows items) <*> get') (M.empty, [])
  let langs = nub (concatMap rowLangs rows ++ concat [concatMap (i18nLangs . choiceLabel) cs | (_, cs) <- lists])
      -- A language may appear only in a hint or a validation message.
      rowLangs r = [l | (k, _) <- M.toList r, base <- ["label", "hint", "constraint_message"], Just l <- [T.stripPrefix (base <> "::") k]]
      i18nCols base = base : [base <> "::" <> l | l <- langs]
      extraCols = nub [k | r <- rows, k <- M.keys r, "x:" `T.isPrefixOf` k]
      surveyCols =
        ["type", "name"] ++ i18nCols "label" ++ i18nCols "hint"
          ++ ["required", "relevant", "constraint"] ++ i18nCols "constraint_message"
          ++ ["calculation", "default", "appearance", "parameters", "choice_filter", "repeat_count", "read_only", "trigger", "media::image", "image", "audio", "video"]
          ++ extraCols
      used cols rs = [c | c <- cols, c `elem` ["type", "name", "label"] || any (not . T.null . cell c) rs]
      defLang = case metaLanguages meta of
        (d : _) -> Just d
        [] -> Nothing
      -- With a declared default language its text goes in label::<lang>.
      header c = case defLang of
        Just d | c `elem` ["label", "hint", "constraint_message"] -> c <> "::" <> d
        _ -> fromMaybe c (T.stripPrefix "x:" c)
      render cols rs = let cs = used cols rs in map header cs : [[cell c r | c <- cs] | r <- rs]
      choiceExtras = case M.lookup choiceColumnsKey (metaAttrs meta) of
        Just v | Success m <- fromJSON v -> m :: M.Map Text (M.Map Text (M.Map Text Text))
        _ -> M.empty
      extrasOf l c = maybe [] M.toList (M.lookup l choiceExtras >>= M.lookup (T.strip (choiceValue c)))
      choiceRows = [M.fromList ([("x:" <> k, v) | (k, v) <- extrasOf l c] ++ ("list_name", l) : ("name", T.strip (choiceValue c)) : i18nRow "label" (choiceLabel c)) | (l, cs) <- lists, c <- cs]
      -- Extra columns in one fixed order, whichever row names them first.
      choiceCols = ["list_name", "name"] ++ i18nCols "label" ++ sort (nub [k | r <- choiceRows, k <- M.keys r, "x:" `T.isPrefixOf` k])
      settingsRow =
        M.fromList $
          [("form_title", i18nDefault t) | Just t <- [metaTitle meta]]
            ++ [("form_id", i) | Just i <- [metaId meta]]
            ++ [("default_language", d) | Just d <- [defLang]]
            ++ [("x:" <> k, v) | (k0, String v) <- M.toList (metaAttrs meta), Just k <- [T.stripPrefix "xlsform:" k0]]
      settingsCols = ["form_title", "form_id", "default_language"] ++ nub [k | k <- M.keys settingsRow, "x:" `T.isPrefixOf` k]
  -- Columns kept for a choice the writer does not write under the same
  -- list (a list merged into an identical one, a choice since removed).
  let written = S.fromList [(l, T.strip (choiceValue c)) | (l, cs) <- lists, c <- cs]
      orphans = nub [l | (l, m) <- M.toList choiceExtras, n <- M.keys m, not (S.member (l, n) written)]
  unless (null orphans) $ warn "xlsform" ("choices sheet columns dropped for lists: " <> T.intercalate ", " orphans)
  pure $
    [("survey", render surveyCols rows), ("choices", render choiceCols choiceRows)]
      ++ [("settings", render settingsCols [settingsRow]) | not (M.null settingsRow)]
  where
    get' = snd <$> get

i18nRow :: Text -> I18n -> [(Text, Text)]
i18nRow base (I18n d tr) = (base, d) : [(base <> "::" <> l, t) | (l, t) <- M.toList tr]

-- | Choice lists already emitted, so identical lists are shared by name.
type W = StateT (M.Map [Choice] Text, [(Text, [Choice])]) Conv

listName :: Text -> [Choice] -> W Text
listName hint0 cs = do
  (seen, acc) <- get
  -- A list name is one word of letters, digits, _ . and -.
  let cleaned = T.map (\c -> if isAlphaNum c || c `elem` ("_.-" :: String) then c else '_') (T.strip hint0)
      hint = if T.null cleaned then "list" else cleaned
  case M.lookup cs seen of
    Just n -> pure n
    Nothing -> do
      let taken = map fst acc
          candidates = hint : [hint <> "_" <> T.pack (show i) | i <- [2 :: Int ..]]
          n = fromMaybe hint (listToMaybe (filter (`notElem` taken) candidates))
      put (M.insert cs n seen, acc ++ [(n, cs)])
      pure n

logic :: Text -> Maybe Expr -> W Text
logic at e = lift (fromMaybe "" <$> printLogic xlsform at e)

itemRows :: Item -> W [Row]
itemRows = \case
  ItemNote i t rel -> do
    r <- logic i rel
    pure [M.fromList ([("type", "note"), ("name", i), ("relevant", r)] ++ i18nRow "label" t)]
  ItemRaw (Format f) _ -> [] <$ lift (warn "xlsform" ("raw " <> f <> " content dropped"))
  ItemGroup g -> do
    r <- logic (groupId g) (groupRelevant g)
    kids <- concat <$> mapM itemRows (groupItems g)
    let kind = if groupKind g == Repeat then "repeat" else "group"
        app = [("appearance", "field-list") | groupKind g == Page]
        begin = M.fromList ([("type", "begin_" <> kind), ("name", groupId g), ("relevant", r)] ++ maybe [] (i18nRow "label") (groupTitle g) ++ attrCols (groupAttrs g) ++ app)
    pure ([begin] ++ kids ++ [M.fromList [("type", "end_" <> kind)]])
  ItemQuestion q -> questionRows q

-- | Preserved columns: known ones by name (so a computed value can
-- override them), others behind an @x:@ marker that places them last.
attrCols :: Attrs -> [(Text, Text)]
attrCols a =
  [ (if k `elem` ["appearance", "parameters", "required", "choice_filter", "repeat_count", "read_only", "trigger", "media::image", "image", "audio", "video", "note"] then k else "x:" <> k, v)
  | (k0, String v) <- M.toList a
  , Just k <- [T.stripPrefix "xlsform:" k0]
  , k `notElem` ["list", "filetype"]
  ]

questionRows :: Question -> W [Row]
questionRows q = do
  rel <- logic (qId q) (qRelevant q)
  con <- logic (qId q) (qValidation q >>= valExpr)
  -- Bounds go in front of the expression, as 'splitBounds' reads them. A
  -- bound on a text question limits its length, not its value. When the
  -- expression cannot be printed (it warned above) the bounds still are.
  let bounds = maybe [] (boundTerms (qType q)) (qValidation q)
      conj = \case
        [] -> Nothing
        (x : xs) -> Just (foldl (EBin And) x xs)
  rangeCon <-
    if null bounds
      then pure con
      else logic (qId q) (conj (bounds ++ [e | not (T.null con), Just e <- [qValidation q >>= valExpr]]))
  let base =
        [ ("name", qId q)
        , ("required", if qRequired q then "yes" else "")
        , ("relevant", rel)
        , ("constraint", rangeCon)
        , ("default", fromMaybe "" (qDefault q))
        ]
          ++ i18nRow "label" (qLabel q)
          ++ maybe [] (i18nRow "hint") (qHint q)
          ++ maybe [] (i18nRow "constraint_message") (qValidation q >>= valMessage)
      -- A kept required expression is written only while the question
      -- is not simply required.
      kept = [(k, v) | (k, v) <- attrCols (qAttrs q), not (k == "required" && qRequired q)]
      row extra = M.fromList (base ++ kept ++ extra)
      note msg = lift (warn (qId q) msg)
  -- An XForm's required expression names questions by their XPath.
  case M.lookup "xform:bind:required" (qAttrs q) of
    Just (String x) | not (qRequired q), not (M.member "xlsform:required" (qAttrs q)) -> note ("required expression " <> x <> " dropped")
    _ -> pure ()
  case qType q of
    QText k -> do
      when (k `elem` [Email, Url]) $ note "email/url input written as plain text"
      pure [row ([("type", "text")] ++ [("appearance", "multiline") | k == LongText] ++ [("appearance", "numbers") | k == Phone])]
    QNumber Integer -> pure [row [("type", "integer")]]
    QNumber Decimal -> pure [row [("type", "decimal")]]
    QChoice sel style cs -> do
      when (any choiceExclusive cs) $ note "exclusive choice written as a plain choice"
      lift (warnFixedCodes (qId q) (Just "other") Nothing cs)
      when (sel == Multiple) $ spaced cs
      when (length (filter choiceOther cs) > 1) $ note "extra \"other\" options merged into or_other"
      let (others, rest) = span' cs
      l <- listName (listHint (qId q)) rest
      let t = (if sel == Single then "select_one " else "select_multiple ") <> l <> (if null others then "" else " or_other")
          app = case style of
            Dropdown -> [("appearance", "minimal")]
            Buttons -> [("appearance", "quick")]
            Radio -> []
      pure [row (("type", t) : app)]
    QRanking cs -> do
      spaced cs
      l <- listName (listHint (qId q)) cs
      pure [row [("type", "rank " <> l)]]
    QMatrix sel rows cols -> do
      note "matrix expanded into a field-list group of one select per row"
      l <- listName (listHint (qId q <> "_scale")) cols
      let ty = (if sel == Single then "select_one " else "select_multiple ") <> l
          header = M.fromList ([("type", "begin_group"), ("name", qId q), ("appearance", "field-list"), ("relevant", rel)] ++ i18nRow "label" (qLabel q))
          sub c = M.fromList ([("type", ty), ("name", qId q <> "_" <> choiceValue c), ("required", if qRequired q then "yes" else ""), ("appearance", "list-nolabel")] ++ i18nRow "label" (choiceLabel c))
      pure ([header] ++ map sub rows ++ [M.fromList [("type", "end_group")]])
    QScale lo hi l h -> do
      when (isJust l || isJust h) $ note "scale end labels dropped"
      -- Parameters read from XLSForm (a step, a decimal range) are kept
      -- while they still describe this scale.
      let params = case M.lookup "xlsform:parameters" (qAttrs q) of
            Just (String p) | rangeEnds p == (lo, hi) -> p
            _ -> "start=" <> tshow lo <> " end=" <> tshow hi <> " step=1"
      pure [row [("type", "range"), ("parameters", params)]]
    QDate Date -> pure [row [("type", "date")]]
    QDate Time -> pure [row [("type", "time")]]
    QDate DateTime -> pure [row [("type", "dateTime")]]
    QBoolean -> do
      note "yes/no written as a select_one with a yes/no list"
      l <- listName "yes_no" [choice "yes" "Yes", choice "no" "No"]
      pure [row [("type", "select_one " <> l)]]
    QFile -> pure [row [("type", fileKind)]]
    QGeo -> pure [row [("type", "geopoint")]]
    QHidden -> pure [row [("type", "hidden")]]
    QCalculate e -> do
      c <- logic (qId q) (Just e)
      pure [row [("type", "calculate"), ("calculation", c)]]
    QOther t
      | T.null (T.strip t) -> [row [("type", "text")]] <$ note "question without a type written as text"
      | otherwise -> pure [row [("type", t)]]
  where
    fileKind = case M.lookup "xlsform:filetype" (qAttrs q) of
      Just (String t) | t `elem` ["image", "audio", "video", "background-audio"] -> t
      _ -> "file"
    listHint d = case M.lookup "xlsform:list" (qAttrs q) of
      Just (String l) | not (T.null l) -> l
      _ -> d
    span' cs = (filter choiceOther cs, filter (not . choiceOther) cs)
    -- A multiple selection and a ranking are stored as space-separated names.
    spaced cs =
      when (any (T.any isSpace . T.strip . choiceValue) cs) $
        lift (warn (qId q) "choice names contain spaces; XLSForm stores several selected names separated by spaces")

tshow :: Show a => a -> Text
tshow = T.pack . show


choicesOf :: Question -> [Choice]
choicesOf q = case qType q of
  QChoice _ _ cs -> cs
  QMatrix _ rs cs -> rs ++ cs
  QRanking cs -> cs
  _ -> []

trimIds :: Item -> Item
trimIds = \case
  ItemGroup g -> ItemGroup g {groupId = T.strip (groupId g), groupItems = map trimIds (groupItems g)}
  ItemQuestion q -> ItemQuestion q {qId = T.strip (qId q)}
  ItemNote i t r -> ItemNote (T.strip i) t r
  i -> i
