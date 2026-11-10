-- | REDCap data dictionary CSV: one row per field, instruments named in
-- "Form Name", choices as @1, Yes | 2, No@, branching logic in REDCap's
-- bracket syntax, and matrices as consecutive fields sharing a
-- "Matrix Group Name".
module Panform.Format.REDCap (readREDCap, writeREDCap) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAsciiLower, isDigit, isSpace)
import Data.List (groupBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Csv
import Panform.Expr

-- | Canonical columns, in REDCap's order.
columns :: [Text]
columns =
  [ "Variable / Field Name"
  , "Form Name"
  , "Section Header"
  , "Field Type"
  , "Field Label"
  , "Choices, Calculations, OR Slider Labels"
  , "Field Note"
  , "Text Validation Type OR Show Slider Number"
  , "Text Validation Min"
  , "Text Validation Max"
  , "Identifier?"
  , "Branching Logic (Show field only if...)"
  , "Required Field?"
  , "Custom Alignment"
  , "Question Number (surveys only)"
  , "Matrix Group Name"
  , "Matrix Ranking?"
  , "Field Annotation"
  ]

-- | Short names for the columns, matched against headers by prefix so minor
-- header variations still read.
data Col = Var | FormName | SectionHdr | FType | Label | Choices | Note | VType | VMin | VMax | Ident | Branch | Req | Align | QNum | MGroup | MRank | Annot
  deriving (Eq, Ord, Enum, Bounded, Show)

colPrefix :: Col -> Text
colPrefix = \case
  Var -> "variable"
  FormName -> "form name"
  SectionHdr -> "section header"
  FType -> "field type"
  Label -> "field label"
  Choices -> "choices"
  Note -> "field note"
  VType -> "text validation type"
  VMin -> "text validation min"
  VMax -> "text validation max"
  Ident -> "identifier"
  Branch -> "branching logic"
  Req -> "required"
  Align -> "custom alignment"
  QNum -> "question number"
  MGroup -> "matrix group"
  MRank -> "matrix ranking"
  Annot -> "field annotation"

-- | A cell; structural columns are trimmed, text columns kept exactly.
get :: Col -> Row -> Text
get c r = case [v | (k, v) <- M.toList r, colPrefix c `T.isPrefixOf` k] of
  (v : _) -> if c `elem` [Label, Choices, Note, SectionHdr, Annot] then v else T.strip v
  [] -> ""

------------------------------------------------------------------------------
-- Reading

readREDCap :: BL.ByteString -> Conv Form
readREDCap bs = do
  rows0 <- readRows ',' bs `orFail` "REDCap data dictionary"
  -- The columns are positional in REDCap; a header that does not name them
  -- (an edited first cell, a translated export) is read by position.
  let named = any (\k -> "variable" `T.isPrefixOf` k) (concatMap M.keys (take 1 rows0))
      rows
        | named || null rows0 = rows0
        | otherwise = map positional (parseRowsByPosition bs)
      positional cells = M.fromList (zip (map T.toLower columns) cells)
  unless (named || null rows0) $ warn "redcap" "header does not name the REDCap columns; read by position"
  when (null rows) $ failWith "REDCap data dictionary is empty"
  forms <- forM (groupBy ((==) `on` get FormName) rows) $ \frows -> do
    let form = maybe "" (get FormName) (listToMaybe frows)
    items <- sections <$> mapM readRow frows
    pure . ItemGroup $ Group form Page (Just (plain (humanise form))) Nothing (mergeMatrices items) M.empty
  pure (Form emptyMeta forms)
  where
    humanise = T.unwords . map T.toTitle . T.splitOn "_"

-- | A field and the section header that precedes it, if any.
data Field = Field (Maybe Text) (Maybe Text) Item

readRow :: Row -> Conv Field
readRow r = do
  let name = get Var r
      label = plain (get Label r)
      ftype = T.toLower (get FType r)
      vtype = T.toLower (get VType r)
      annot = get Annot r
      relevant = parseOrRaw redcap <$> nonEmpty (get Branch r)
      cs = parseChoices (get Choices r)
  qt <- case ftype of
    "text"
      | "@hidden" `T.isInfixOf` T.toLower annot -> pure QHidden
      | otherwise -> pure $ case vtype of
          "integer" -> QNumber Integer
          "number" -> QNumber Decimal
          "email" -> QText Email
          "phone" -> QText Phone
          t | "datetime" `T.isPrefixOf` t -> QDate DateTime
          t | "date" `T.isPrefixOf` t -> QDate Date
          t | "time" `T.isPrefixOf` t -> QDate Time
          t | "number" `T.isPrefixOf` t -> QNumber Decimal
          _ -> QText ShortText
    "notes" -> pure (QText LongText)
    "radio" -> pure (QChoice Single Radio cs)
    "dropdown" -> pure (QChoice Single Dropdown cs)
    "checkbox" -> pure (QChoice Multiple Radio [c {choiceExclusive = choiceValue c `elem` noneOf annot} | c <- cs])
    "yesno" -> pure QBoolean
    "truefalse" -> pure QBoolean
    "calc" -> pure (QCalculate (parseOrRaw redcap (get Choices r)))
    "file" -> pure QFile
    -- Slider labels are positional: left | middle | right.
    "slider" -> pure (QScale (maybe 0 round (num (get VMin r))) (maybe 100 round (num (get VMax r))) (plain <$> sliderLabel 0) (plain <$> sliderLabel 2))
    "descriptive" -> pure (QOther "descriptive")
    other -> QOther other <$ warn name ("unsupported field type " <> other)
  -- A minimum or maximum is a bound on a number's value. On anything
  -- else (a date, a time, a hidden or plain text field) it is kept as
  -- written: a bound on a text answer would be read as its length.
  let isNumber = case qt of
        QNumber _ -> True
        _ -> False
      bound c = if ftype == "text" && isNumber then num (get c r) else Nothing
      validation
        | isJust (bound VMin) || isJust (bound VMax) = Just (Validation (bound VMin) (bound VMax) Nothing Nothing)
        | otherwise = Nothing
      rawBound c = if ftype == "text" && not (isJust (bound c)) then get c r else ""
      attrs =
        M.fromList
          [ ("redcap:" <> k, String v)
          | (k, v) <-
              [ ("annotation", stripNone annot)
              , ("identifier", get Ident r)
              , ("alignment", get Align r)
              , ("validation", vtype)
              , ("validation_min", rawBound VMin)
              , ("validation_max", rawBound VMax)
              , ("question_number", get QNum r)
              , ("matrix_ranking", get MRank r)
              , ("slider_middle", if ftype == "slider" then fromMaybe "" (sliderLabel 1) else "")
              , -- "number" shows the slider's value; the writer's default.
                ("slider_number", if ftype == "slider" && vtype /= "number" then (if T.null vtype then "off" else vtype) else "")
              ]
          , not (T.null v)
          , not (k == "validation" && ftype /= "text")
          ]
      item = case qt of
        QOther "descriptive" -> ItemNote name label relevant
        _ ->
          ItemQuestion
            Question
              { qId = name
              , qLabel = label
              , qHint = plain <$> nonEmpty (get Note r)
              , qType = qt
              , qRequired = T.toLower (get Req r) `elem` ["y", "yes", "1"]
              , qRelevant = relevant
              , qValidation = validation
              , qDefault = Nothing
              , qAttrs = attrs
              }
  pure (Field (nonEmpty (get SectionHdr r)) (nonEmpty (get MGroup r)) item)
  where
    num :: Text -> Maybe Double
    num t = case reads (T.unpack t) of
      [(d, "")] -> Just d
      _ -> Nothing
    sliderLabel i = case drop i (map T.strip (T.splitOn "|" (get Choices r))) of
      (l : _) | not (T.null l) -> Just l
      _ -> Nothing

-- | Codes named by a @\@NONEOFTHEABOVE='98,99'@ action tag.
noneOf :: Text -> [Text]
noneOf annot = case T.breakOn "@NONEOFTHEABOVE" (T.toUpper annot) of
  (pre, rest) | not (T.null rest) ->
    let after = T.stripStart (T.drop (T.length pre + T.length "@NONEOFTHEABOVE") annot)
        value = case T.uncons after of
          Just ('=', v) -> T.takeWhile (\c -> c /= '\'' && c /= '"') (T.dropWhile (\c -> c == '\'' || c == '"' || c == ' ') v)
          _ -> ""
     in filter (not . T.null) (map T.strip (T.splitOn "," value))
  _ -> []

-- | The annotation without its @\@NONEOFTHEABOVE@ tag, which the choices carry.
stripNone :: Text -> Text
stripNone annot = case T.breakOn "@NONEOFTHEABOVE" (T.toUpper annot) of
  (pre, rest) | not (T.null rest) ->
    let before = T.take (T.length pre) annot
        after = T.stripStart (T.drop (T.length pre + T.length "@NONEOFTHEABOVE") annot)
        remaining = case T.uncons after of
          Just ('=', v) ->
            let v' = T.stripStart v
             in case T.uncons v' of
                  Just (q, body) | q `elem` ['\'', '"'] -> T.drop 1 (T.dropWhile (/= q) body)
                  _ -> T.dropWhile (not . isSpace) v'
          _ -> after
     in T.unwords (T.words (before <> " " <> remaining))
  _ -> annot

-- | Data rows (after the header) as positional cells.
parseRowsByPosition :: BL.ByteString -> [[Text]]
parseRowsByPosition = drop 1 . parseCells ','

-- | "1, Yes | 2, No" (a missing code uses the label as the code).
parseChoices :: Text -> [Choice]
parseChoices t =
  [ case T.breakOn "," part of
      (code, rest) | not (T.null rest) -> choice (T.strip code) (T.strip (T.drop 1 rest))
      _ -> choice (T.strip part) (T.strip part)
  | part <- T.splitOn "|" t
  , not (T.null (T.strip part))
  ]

-- | Section headers open a 'Section' that runs to the next header.
sections :: [Field] -> [(Maybe Text, Item)]
sections = go
  where
    go [] = []
    go (Field (Just h) m i : rest) =
      let (inSec, after) = break (\(Field h' _ _) -> isJust h') rest
          sid = "section_" <> itemId i
       in (Nothing, ItemGroup (Group sid Section (Just (plain h)) Nothing (map snd' ((m, i) : map unF inSec)) M.empty)) : go after
    go (Field Nothing m i : rest) = (m, i) : go rest
    unF (Field _ m i) = (m, i)
    snd' (m, i) = tag m i

-- | Matrix membership is carried on the item until merged.
tag :: Maybe Text -> Item -> Item
tag (Just m) (ItemQuestion q) = ItemQuestion q {qAttrs = M.insert "redcap:matrix" (String m) (qAttrs q)}
tag _ i = i

-- | Consecutive fields of one matrix group with the same choices become a
-- single 'QMatrix'.
mergeMatrices :: [(Maybe Text, Item)] -> [Item]
mergeMatrices = concatMap merge . groupBy sameGroup . map (uncurry tag)
  where
    groupOf (ItemQuestion q) = M.lookup "redcap:matrix" (qAttrs q)
    groupOf _ = Nothing
    sameGroup a b = isJust (groupOf a) && groupOf a == groupOf b
    merge is@(ItemQuestion q : _)
      | Just (String g) <- groupOf (ItemQuestion q)
      , Just (sel, cols) <- scale (qType q)
      , all (\i -> fmap snd (scaleOf i) == Just cols) is
      -- Rows that differ in anything but their name and label (a note,
      -- logic, a required flag, an annotation) stay questions of their
      -- own, in their matrix group.
      , all (\i -> fmap rowShape (asQ i) == Just (rowShape q)) is =
          [ ItemQuestion
              q
                { qId = g
                , qLabel = plain ""
                , qType = QMatrix sel [Choice (fromMaybe (qId r) (T.stripPrefix (g <> "_") (qId r))) (qLabel r) False False | ItemQuestion r <- is] cols
                , qAttrs =
                    -- Row variables not named <group>_<row> keep their own names.
                    (if all (\i -> maybe False ((g <> "_") `T.isPrefixOf`) (idOf i)) is then id else M.insert "redcap:row_names" (String "own"))
                      (M.delete "redcap:matrix" (qAttrs q))
                }
          ]
    merge is = map untag is
    asQ (ItemQuestion r) = Just r
    asQ _ = Nothing
    rowShape r = r {qId = "", qLabel = plain ""}
    idOf (ItemQuestion r) = Just (qId r)
    idOf _ = Nothing
    scaleOf (ItemQuestion q) = scale (qType q)
    scaleOf _ = Nothing
    scale (QChoice s _ cs) = Just (s, cs)
    scale _ = Nothing
    untag (ItemGroup g) = ItemGroup g {groupItems = mergeMatrices (map (Nothing,) (groupItems g))}
    untag i = i

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null (T.strip t) then Nothing else Just t

------------------------------------------------------------------------------
-- Writing

writeREDCap :: Form -> Conv BL.ByteString
writeREDCap (Form meta items) = do
  let formName = maybe "survey" (slug . i18nDefault) (metaTitle meta)
  rows <- concat <$> mapM (itemRows formName Nothing) items
  let isId r = M.lookup "Variable / Field Name" r == Just "record_id"
      firstForm = maybe formName (M.findWithDefault formName "Form Name") (safeHead rows)
      idRow = M.fromList [("Variable / Field Name", "record_id"), ("Form Name", firstForm), ("Field Type", "text"), ("Field Label", "Record ID")]
  case break isId rows of
    -- Already first.
    ([], _ : _) -> pure (writeRows ',' columns rows)
    (_, []) -> do
      warn "redcap" "added a record_id field; REDCap requires the first field to be the record identifier"
      pure (writeRows ',' columns (idRow : rows))
    -- A second record_id would be a duplicate variable: the survey's own
    -- moves to the front, and a section header it opened stays behind.
    (before, r : after) -> do
      warn "record_id" "moved to the first field; REDCap requires the first field to be the record identifier"
      let hdr = M.findWithDefault "" "Section Header" r
          after' = case after of
            (n : more) | not (T.null hdr), T.null (M.findWithDefault "" "Section Header" n), M.lookup "Form Name" n == M.lookup "Form Name" r -> M.insert "Section Header" hdr n : more
            _ -> after
      when (not (T.null hdr) && after' == after) $ warn "record_id" "its section header dropped"
      pure (writeRows ',' columns (M.insert "Section Header" "" (M.insert "Form Name" firstForm r) : before ++ after'))
  where
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing

slug :: Text -> Text
slug t =
  let s = T.intercalate "_" (T.words (T.map (\c -> if isAlphaNum c then c else ' ') (T.toLower t)))
   in if T.null s then "survey" else s

validName :: Text -> Bool
validName n = not (T.null n) && T.all (\c -> isAsciiLower c || isDigit c || c == '_') n && maybe False (isAsciiLower . fst) (T.uncons n)

itemRows :: Text -> Maybe Text -> Item -> Conv [Row]
itemRows form header item0 = case item0 of
  ItemGroup g -> case groupKind g of
    Page -> do
      unless (groupRelevant g == Nothing) $ warn (groupId g) "instrument-level logic dropped (REDCap sets it per event)"
      concat <$> mapM (itemRows (slug (groupId g)) Nothing) (groupItems g)
    _ -> do
      let hdr = maybe (Just (groupId g)) (Just . i18nDefault) (groupTitle g)
          kids = map (withRelevant (groupRelevant g)) (groupItems g)
      rows <- concat <$> mapM (itemRows form Nothing) kids
      -- The first field may open with a header of its own (a matrix's
      -- label, a nested section's title): one cell holds both.
      let own = maybe "" (M.findWithDefault "" "Section Header") (listToMaybe rows)
          title = fromMaybe "" (combine header hdr)
      unless (T.null own) $ warn (groupId g) ("section title and the header of its first field written as one section header")
      when (groupKind g == Repeat) $ warn (groupId g) "repeat written as a section; REDCap sets repeating instruments in the project, not the dictionary"
      pure (onFirst (M.insert "Section Header" (if T.null own then title else title <> " " <> own)) rows)
  ItemNote i t rel -> do
    b <- printLogic redcap i rel
    pure [base i (i18nDefault t) "descriptive" b]
  ItemRaw (Format f) _ -> [] <$ warn "redcap" ("raw " <> f <> " content dropped")
  ItemQuestion q -> do
    unless (validName (qId q)) $ warn (qId q) "REDCap variable names must be lower-case letters, digits and underscores"
    b <- printLogic redcap (qId q) (qRelevant q)
    translationsWarn q
    let padded c = let l = i18nDefault (choiceLabel c) in l /= T.strip l || T.any (== '|') l || choiceCode (choiceValue c) /= choiceValue c
        allChoices = case qType q of
          QChoice _ _ cs -> cs
          QMatrix _ rs cs -> rs ++ cs
          QRanking cs -> cs
          _ -> []
    when (any padded allChoices) $ warn (qId q) "choice codes/labels trimmed; REDCap's \"code, label | ...\" list cannot hold surrounding spaces or '|'"
    let r0 = base (qId q) (i18nDefault (qLabel q)) "" b
        withCommon r =
          M.union r . M.fromList $
            [("Required Field?", "y") | qRequired q]
              ++ [("Field Note", i18nDefault h) | Just h <- [qHint q]]
              ++ [("Field Annotation", v) | Just (String v) <- [M.lookup "redcap:annotation" (qAttrs q)]]
              ++ [("Identifier?", v) | Just (String v) <- [M.lookup "redcap:identifier" (qAttrs q)]]
              ++ [("Custom Alignment", v) | Just (String v) <- [M.lookup "redcap:alignment" (qAttrs q)]]
              ++ [("Question Number (surveys only)", v) | Just (String v) <- [M.lookup "redcap:question_number" (qAttrs q)]]
              ++ [("Matrix Ranking?", v) | Just (String v) <- [M.lookup "redcap:matrix_ranking" (qAttrs q)]]
              ++ [("Matrix Group Name", v) | Just (String v) <- [M.lookup "redcap:matrix" (qAttrs q)]]
        set ty extra = [withCommon (M.union (M.fromList (("Field Type", ty) : extra)) r0)]
        -- Bounds on a number's value, then any bound kept as it was read.
        valueRng = case qValidation q of
          Just v -> [("Text Validation Min", n) | Just n <- [showNum' <$> valMin v]] ++ [("Text Validation Max", n) | Just n <- [showNum' <$> valMax v]]
          Nothing -> []
        keptRng = [("Text Validation Min", v) | Just v <- [itemAttr "redcap:validation_min"]] ++ [("Text Validation Max", v) | Just v <- [itemAttr "redcap:validation_max"]]
        rng = valueRng ++ [kv | kv <- keptRng, fst kv `notElem` map fst valueRng]
        bounded = isJust (qValidation q >>= \v -> maybe (valMax v) Just (valMin v))
    when (isJust (qValidation q >>= valExpr)) $ warn (qId q) "validation expression dropped"
    case qType q of
      QNumber _ -> pure ()
      QText _ | bounded -> warn (qId q) "text length limit dropped; REDCap checks a value's range, not its length"
      _ | bounded -> warn (qId q) "value range dropped"
      _ -> pure ()
    case qType q of
      QText ShortText -> pure (set "text" ([("Text Validation Type OR Show Slider Number", v) | Just v <- [itemAttr "redcap:validation"]] ++ keptRng))
      QText LongText -> pure (set "notes" [])
      QText Email -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "email") : keptRng))
      QText Phone -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "phone") : keptRng))
      QText Url -> set "text" [] <$ warn (qId q) "URL validation dropped"
      QNumber Integer -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "integer") : rng))
      QNumber Decimal -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "number") : rng))
      QDate Date -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "date_ymd") : keptRng))
      QDate Time -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "time") : keptRng))
      QDate DateTime -> pure (set "text" (("Text Validation Type OR Show Slider Number", keep "datetime_ymd") : keptRng))
      QChoice sel style cs -> do
        when (any choiceOther cs) $ warn (qId q) "\"other, specify\" text box dropped"
        let ty = case (sel, style) of
              (Multiple, _) -> "checkbox"
              (Single, Dropdown) -> "dropdown"
              _ -> "radio"
        let none = [choiceValue c | c <- cs, choiceExclusive c, sel == Multiple]
            tags = T.unwords (["@NONEOFTHEABOVE='" <> T.intercalate "," none <> "'" | not (null none)] ++ [v | Just (String v) <- [M.lookup "redcap:annotation" (qAttrs q)]])
        pure [M.insert "Field Annotation" tags r | r <- set ty [("Choices, Calculations, OR Slider Labels", choicesText cs)]]
      QMatrix sel rows cols ->
        pure
          [ withCommon . M.fromList $
              [ ("Variable / Field Name", rowName row)
              , ("Form Name", form)
              , ("Field Type", if sel == Single then "radio" else "checkbox")
              , ("Field Label", i18nDefault (choiceLabel row))
              , ("Choices, Calculations, OR Slider Labels", choicesText cols)
              , ("Matrix Group Name", T.strip (qId q))
              , ("Branching Logic (Show field only if...)", fromMaybe "" b)
              ]
                ++ [("Section Header", i18nDefault (qLabel q)) | n == (0 :: Int), not (T.null (i18nDefault (qLabel q)))]
          | (n, row) <- zip [0 ..] rows
          ]
      QScale lo hi l h -> do
        let labels = T.intercalate " | " [maybe "" (T.strip . i18nDefault) l, fromMaybe "" (itemAttr "redcap:slider_middle"), maybe "" (T.strip . i18nDefault) h]
            shown = case itemAttr "redcap:slider_number" of
              Just "off" -> ""
              Just v -> v
              Nothing -> "number"
        pure (set "slider" [("Choices, Calculations, OR Slider Labels", labels), ("Text Validation Min", tshow lo), ("Text Validation Max", tshow hi), ("Text Validation Type OR Show Slider Number", shown)])
      QRanking cs -> do
        warn (qId q) "ranking written as a set of dropdowns, one per rank"
        pure
          [ withCommon . M.fromList $
              [ ("Variable / Field Name", qId q <> "_rank" <> tshow n)
              , ("Form Name", form)
              , ("Field Type", "dropdown")
              , ("Field Label", "Rank " <> tshow n)
              , ("Choices, Calculations, OR Slider Labels", choicesText cs)
              ]
          | n <- [1 .. length cs]
          ]
      QBoolean -> pure (set "yesno" [])
      QFile -> pure (set "file" [])
      QGeo -> set "text" [] <$ warn (qId q) "geopoint written as a text field"
      -- An annotation that already hides the field (@HIDDEN-SURVEY with
      -- its other tags) is written as it was read.
      QHidden ->
        let tags = case itemAttr "redcap:annotation" of
              Just a | "@hidden" `T.isInfixOf` T.toLower a -> a
              Just a -> "@HIDDEN " <> a
              Nothing -> "@HIDDEN"
         in pure [M.insert "Field Annotation" tags r | r <- set "text" ([("Text Validation Type OR Show Slider Number", v) | Just v <- [itemAttr "redcap:validation"]] ++ keptRng)]
      QCalculate e -> do
        c <- printLogic redcap (qId q) (Just e)
        pure (set "calc" [("Choices, Calculations, OR Slider Labels", fromMaybe "" c)])
      QOther t -> set "text" [] <$ warn (qId q) ("type " <> t <> " written as a text field")
  where
    -- The validation type a read kept, when it is the same kind as the
    -- generic one (date_dmy for date_ymd, datetime_seconds_ymd for
    -- datetime_ymd, number_2dp for number).
    rowName row = case itemAttr "redcap:row_names" of
      Just "own" -> choiceCode (choiceValue row)
      _ -> case item0 of
        ItemQuestion q -> T.strip (qId q) <> "_" <> choiceCode (choiceValue row)
        _ -> choiceCode (choiceValue row)
    keep generic = case itemAttr "redcap:validation" of
      Just v | kind v == kind generic -> v
      _ -> generic
    kind v
      | "datetime" `T.isPrefixOf` v = "datetime"
      | "date" `T.isPrefixOf` v = "date"
      | "time" `T.isPrefixOf` v = "time"
      | "number" `T.isPrefixOf` v = "number"
      | otherwise = v
    itemAttr k = case item0 of
      ItemQuestion q | Just (String v) <- M.lookup k (qAttrs q) -> Just v
      _ -> Nothing
    base i label ty b =
      M.fromList
        [ ("Variable / Field Name", T.strip i)
        , ("Form Name", form)
        , ("Field Type", ty)
        , ("Field Label", label)
        , ("Branching Logic (Show field only if...)", fromMaybe "" b)
        , ("Section Header", fromMaybe "" header)
        ]
    onFirst f (r : rs) = f r : rs
    onFirst _ [] = []
    combine a b = case (a, b) of
      (Just x, Just y) -> Just (x <> " " <> y)
      _ -> maybe b Just a
    choicesText cs = T.intercalate " | " [choiceCode (choiceValue c) <> ", " <> T.strip (i18nDefault (choiceLabel c)) | c <- cs]
    showNum' x = if x == fromIntegral (round x :: Integer) then tshow (round x :: Integer) else tshow x

-- | Group logic has nowhere to live in REDCap, so it is pushed onto each child.
withRelevant :: Maybe Expr -> Item -> Item
withRelevant Nothing i = i
withRelevant (Just e) i = case i of
  ItemQuestion q -> ItemQuestion q {qRelevant = Just (maybe e (EBin And e) (qRelevant q))}
  ItemNote n t r -> ItemNote n t (Just (maybe e (EBin And e) r))
  ItemGroup g -> ItemGroup g {groupRelevant = Just (maybe e (EBin And e) (groupRelevant g))}
  _ -> i

translationsWarn :: Question -> Conv ()
translationsWarn q =
  unless (null (i18nLangs (qLabel q))) $
    warn (qId q) "translations dropped (REDCap keeps them in the Multi-Language Management module)"

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | A choice code as REDCap's "code, label | ..." list can hold it: no
-- surrounding spaces, and no comma or bar (each becomes an underscore).
choiceCode :: Text -> Text
choiceCode = T.map (\c -> if c == ',' || c == '|' then '_' else c) . T.strip
