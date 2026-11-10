-- | Qualtrics TXT import in the Advanced Format: a plain-text file of
-- @[[Tag]]@ lines. @[[Block:name]]@ and @[[PageBreak]]@ structure it,
-- @[[Question:MC:SingleAnswer:Vertical]]@ starts a question, @[[ID:x]]@
-- sets its export tag, and @[[Choices]]@ / @[[AdvancedChoices]]@ /
-- @[[Answers]]@ list options. There is no logic, validation or translation.
module Panform.Format.QualtricsTxt (readQualtricsTxt, writeQualtricsTxt) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..))
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as BL
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Read as TR
import Panform.AST
import Panform.Conv
import Panform.Format.Codebook (maxScalePoints, scaleTooWide)
import Panform.Format.Qualtrics (selectorFor)

------------------------------------------------------------------------------
-- Reading

-- | A line is a tag when it is exactly @[[...]]@.
tagOf :: Text -> Maybe Text
tagOf l = T.stripPrefix "[[" (T.strip l) >>= T.stripSuffix "]]"

data Top
  = TBlock (Maybe Text)
  | TBreak
  | TED Text (Maybe Text)
  | TQuestion Text [Text]
  -- ^ Type tag and the lines up to the next top-level tag.

isTopTag :: Text -> Bool
isTopTag t =
  t `elem` ["AdvancedFormat", "Block", "PageBreak"]
    || any (`T.isPrefixOf` t) ["Block:", "Question:", "ED:"]
    || t == "Question"

tops :: [Text] -> Conv [Top]
tops [] = pure []
tops (l : rest) = case tagOf l of
  Just t | isTopTag t -> do
    let (body, rest') = break (maybe False isTopTag . tagOf) rest
    here <- case T.breakOn ":" t of
      ("AdvancedFormat", _) -> pure []
      ("Block", "") -> pure [TBlock Nothing]
      ("Block", name) -> pure [TBlock (Just (T.drop 1 name))]
      ("PageBreak", _) -> pure [TBreak]
      ("ED", fv) -> pure [uncurry TED (splitED (T.drop 1 fv))]
      ("Question", ty) -> pure [TQuestion (T.drop 1 ty) body]
      _ -> [] <$ warn "qualtrics-txt" ("tag " <> t <> " dropped")
    let stray = [b | b <- body, not (T.null (T.strip b))]
    case here of
      [TQuestion {}] -> (here ++) <$> tops rest'
      _ -> do
        unless (null stray) $ warn "qualtrics-txt" ("text outside a question dropped: " <> T.strip (head' stray))
        (here ++) <$> tops rest'
  Just t -> warn "qualtrics-txt" ("tag " <> t <> " outside a question dropped") >> tops rest
  Nothing
    | T.null (T.strip l) -> tops rest
    | otherwise -> warn "qualtrics-txt" ("text outside a question dropped: " <> T.strip l) >> tops rest
  where
    splitED fv = case T.breakOn ":" fv of
      (f, "") -> (f, Nothing)
      (f, v) -> (f, Just (T.drop 1 v))
    head' (x : _) = x
    head' [] = ""

readQualtricsTxt :: BL.ByteString -> Conv Form
readQualtricsTxt bs = do
  let src = T.replace "\r\n" "\n" (TE.decodeUtf8With TE.lenientDecode (BL.toStrict bs))
      ls = dropWhile (T.null . T.strip) (T.lines (fromMaybe src (T.stripPrefix "\xFEFF" src)))
  case ls of
    (l : _) | tagOf l == Just "AdvancedFormat" -> pure ()
    _ -> warn "qualtrics-txt" "no [[AdvancedFormat]] header, read as Advanced Format anyway"
  ts <- tops ls
  items <- assemble ts
  pure (Form emptyMeta items)

-- | Pages and top-level embedded data, in order.
assemble :: [Top] -> Conv [Item]
assemble = go (0 :: Int) (0 :: Int) (1 :: Int) Nothing []
  where
    -- block number, questions so far, page within block, open page, done
    go _ _ _ open acc [] = pure (reverse (close open acc))
    go b nq p open acc (t : rest) = case t of
      TBlock name ->
        go (b + 1) nq 1 (Just (Group (blockId (b + 1)) Page (plain <$> name) Nothing [] M.empty)) (close open acc) rest
      TBreak ->
        let b' = max b 1
            g = Group (blockId b' <> "-" <> T.pack (show (p + 1))) Page Nothing Nothing [] (M.singleton "qualtrics:block" (String (blockId b')))
         in go b' nq (p + 1) (Just g) (close (Just (fromMaybe (Group (blockId b') Page Nothing Nothing [] M.empty) open)) acc) rest
      TED f v ->
        let q = ItemQuestion (question f f QHidden) {qDefault = v}
         in case open of
              Just g -> go b nq p (Just g {groupItems = groupItems g ++ [q]}) acc rest
              Nothing -> go b nq p open (q : acc) rest
      TQuestion ty body -> do
        item <- readQuestion (nq + 1) ty body
        let g = fromMaybe (Group (blockId 1) Page Nothing Nothing [] M.empty) open
            b' = max b 1
        go b' (nq + 1) p (Just g {groupItems = groupItems g ++ [item]}) acc rest
    close Nothing acc = acc
    close (Just g) acc = ItemGroup g : acc
    blockId :: Int -> Text
    blockId n = "block" <> T.pack (show n)

data Mode = MText | MChoices | MAdvChoices | MAnswers | MAdvAnswers
  deriving (Eq)

data Parsed = Parsed
  { pId :: Maybe Text
  , pMulti :: Bool
  , pText :: [Text]
  , pChoices :: [(Maybe Text, [Text])]
  , pAnswers :: [(Maybe Text, [Text])]
  }

parseBody :: Text -> [Text] -> Conv Parsed
parseBody at = go MText (Parsed Nothing False [] [] [])
  where
    go _ p [] = pure p
    go m p (l : rest) = case tagOf l of
      Just t -> case T.breakOn ":" t of
        ("ID", i) -> go m p {pId = Just (T.strip (T.drop 1 i))} rest
        ("MultipleAnswer", _) -> go m p {pMulti = True} rest
        ("Choices", _) -> go MChoices p rest
        ("AdvancedChoices", _) -> go MAdvChoices p rest
        ("Answers", _) -> go MAnswers p rest
        ("AdvancedAnswers", _) -> go MAdvAnswers p rest
        ("Choice", v) | m == MAdvChoices -> go m p {pChoices = pChoices p ++ [(code v, [])]} rest
        ("Answer", v) | m == MAdvAnswers -> go m p {pAnswers = pAnswers p ++ [(code v, [])]} rest
        _ -> warn at ("tag " <> t <> " dropped") >> go m p rest
      Nothing -> case m of
        MText -> go m p {pText = pText p ++ [l]} rest
        MChoices
          | T.null (T.strip l) -> go m p rest
          | otherwise -> go m p {pChoices = pChoices p ++ [(Nothing, [T.strip l])]} rest
        MAnswers
          | T.null (T.strip l) -> go m p rest
          | otherwise -> go m p {pAnswers = pAnswers p ++ [(Nothing, [T.strip l])]} rest
        MAdvChoices -> go m p {pChoices = appendLine l (pChoices p)} rest
        MAdvAnswers -> go m p {pAnswers = appendLine l (pAnswers p)} rest
    code v = if T.null v then Nothing else Just (T.strip (T.drop 1 v))
    appendLine l cs = case reverse cs of
      (c, ls) : before -> reverse ((c, ls ++ [l]) : before)
      [] -> if T.null (T.strip l) then [] else [(Nothing, [l])]

-- | Text lines without surrounding blank lines.
block :: [Text] -> Text
block = T.intercalate "\n" . dropWhileEnd (T.null . T.strip) . dropWhile (T.null . T.strip) . map (T.dropWhileEnd (== ' '))

toChoices :: [(Maybe Text, [Text])] -> [Choice]
toChoices cs = [Choice (fromMaybe (T.pack (show n)) v) (plain (block ls)) False False | (n, (v, ls)) <- zip [1 :: Int ..] cs]

readQuestion :: Int -> Text -> [Text] -> Conv Item
readQuestion n ty body = do
  let parts = T.splitOn ":" ty
      at = "question " <> T.pack (show n)
  p <- parseBody at body
  let tag = fromMaybe ("Q" <> T.pack (show n)) (pId p)
      label = plain (block (pText p))
      base = (question tag "" (QOther ty)) {qLabel = label}
      withSel sel q = q {qAttrs = M.insert "qualtrics:selector" (String sel) (qAttrs q)}
      has x = x `elem` parts
  case parts of
    (t : _)
      | t == "MC" -> do
          let multi = pMulti p || has "MultipleAnswer" || has "MultiSelect"
              sel
                | has "Dropdown" = "DL"
                | has "Select" = "SB"
                | has "MultiSelect" = "MSB"
                | multi = if has "Horizontal" then "MAHR" else "MAVR"
                | otherwise = if has "Horizontal" then "SAHR" else "SAVR"
              s = if multi then Multiple else Single
              style = if sel `elem` ["DL", "SB", "MSB"] then Dropdown else Radio
              q = base {qType = QChoice s style (toChoices (pChoices p))}
          pure (ItemQuestion (if selectorFor s style == sel then q else withSel sel q))
      | t == "Matrix" ->
          pure (ItemQuestion base {qType = QMatrix (if has "MultipleAnswer" then Multiple else Single) (toChoices (pChoices p)) (toChoices (pAnswers p))})
      | t `elem` ["TE", "TextEntry"] ->
          if has "Form"
            then do
              warn tag "form expanded into one text question per field"
              pure . ItemGroup $
                Group
                  { groupId = tag
                  , groupKind = Section
                  , groupTitle = Just label
                  , groupRelevant = Nothing
                  , groupItems = [ItemQuestion (question (tag <> "_" <> choiceValue c) "" (QText ShortText)) {qLabel = choiceLabel c} | c <- toChoices (pChoices p)]
                  , groupAttrs = M.singleton "qualtrics:form" (Bool True)
                  }
            else pure (ItemQuestion base {qType = QText (if has "Essay" then LongText else ShortText)})
      | t `elem` ["RO", "RankOrder"] -> pure (ItemQuestion base {qType = QRanking (toChoices (pChoices p))})
      | t `elem` ["Text", "DB"] -> pure (ItemNote tag label Nothing)
    _ -> do
      warn tag ("unsupported question type " <> ty <> " kept by name")
      pure (ItemQuestion base {qAttrs = M.singleton "qualtrics:txt" (String (T.intercalate "\n" (("[[Question:" <> ty <> "]]") : dropWhileEnd (T.null . T.strip) body)))})

------------------------------------------------------------------------------
-- Writing

writeQualtricsTxt :: Form -> Conv BL.ByteString
writeQualtricsTxt (Form meta items) = do
  let hidden = [q | ItemQuestion q <- allItems items, isHidden q]
      texts = concatMap itemTexts (allItems items) ++ maybe [] pure (metaTitle meta)
  when (any (not . M.null . i18nTranslations) texts) $ warn "qualtrics-txt" "translations dropped, the format has one language"
  when (isJust (metaTitle meta)) $ warn "qualtrics-txt" "survey title dropped"
  eds <- forM hidden $ \q -> do
    case qType q of
      QCalculate _ -> warn (qId q) "calculation dropped, kept as an embedded data field"
      _ -> pure ()
    when (isJust (qRelevant q)) $ warn (qId q) "logic dropped"
    when (qLabel q /= plain (qId q)) $ warn (qId q) "embedded data field label dropped"
    pure ["[[ED:" <> qId q <> maybe "" (":" <>) (qDefault q) <> "]]"]
  body <- topLevel items
  let chunks = [["[[AdvancedFormat]]"]] ++ eds ++ body
  pure (BL.fromStrict (TE.encodeUtf8 (T.intercalate "\n\n" (map (T.intercalate "\n") chunks) <> "\n")))
  where
    itemTexts = \case
      ItemQuestion q -> qLabel q : maybe [] pure (qHint q) ++ choiceTexts (qType q)
      ItemNote _ t _ -> [t]
      ItemGroup g -> maybe [] pure (groupTitle g)
      ItemRaw _ _ -> []
    choiceTexts = \case
      QChoice _ _ cs -> map choiceLabel cs
      QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
      QRanking cs -> map choiceLabel cs
      _ -> []

isHidden :: Question -> Bool
isHidden q = case qType q of
  QHidden -> True
  QCalculate _ -> True
  _ -> False

-- | Blocks and page breaks; loose items go in an unnamed block.
topLevel :: [Item] -> Conv [[Text]]
topLevel = go Nothing
  where
    go _ [] = pure []
    go prev (i : rest) = case i of
      ItemGroup g -> do
        unless (groupKind g == Page) $ warn (groupId g) "top-level group written as a block"
        when (isJust (groupRelevant g)) $ warn (groupId g) "logic dropped"
        let continues = isJust prev && M.lookup "qualtrics:block" (groupAttrs g) == fmap String prev
            header
              | continues = "[[PageBreak]]"
              | otherwise = maybe "[[Block]]" (\t -> "[[Block:" <> T.replace "\n" " " (i18nDefault t) <> "]]") (groupTitle g)
        when (continues && isJust (groupTitle g)) $ warn (groupId g) "page title dropped"
        kids <- concat <$> mapM (writeItem Nothing) (groupItems g)
        let blockName = if continues then prev else Just (groupId g)
        ([header] :) . (kids ++) <$> go blockName rest
      _ | isLoose i -> do
        let (loose, rest') = span isLoose (i : rest)
        kids <- concat <$> mapM (writeItem Nothing) loose
        if null kids then go prev rest' else ([["[[Block]]"]] ++) . (kids ++) <$> go Nothing rest'
      ItemRaw (Format f) _ -> warn "qualtrics-txt" ("raw " <> f <> " content dropped") >> go prev rest
      _ -> go prev rest
    isLoose = \case
      ItemQuestion q -> not (isHidden q)
      ItemNote {} -> True
      _ -> False

writeItem :: Maybe Expr -> Item -> Conv [[Text]]
writeItem rel = \case
  ItemQuestion q
    | isHidden q -> pure []
    | otherwise -> pure <$> writeQuestion q {qRelevant = qRelevant q `orE` rel}
  ItemNote i t r -> do
    checkId i
    checkText i t
    when (isJust (r `orE` rel)) $ warn i "logic dropped"
    pure [["[[Question:Text]]", idLine i, txt t]]
  ItemGroup g
    | M.lookup "qualtrics:form" (groupAttrs g) == Just (Bool True)
    , Just fields <- mapM textField (groupItems g) -> do
        when (isJust (groupRelevant g `orE` rel)) $ warn (groupId g) "logic dropped"
        checkId (groupId g)
        mapM_ (checkText (groupId g)) (maybe [] pure (groupTitle g))
        let cs = [Choice (T.pack (show n)) l False False | (n, l) <- zip [1 :: Int ..] fields]
        ls <- choiceLines (groupId g) "Choice" cs
        pure [["[[Question:TE:Form]]", idLine (groupId g), maybe "" txt (groupTitle g)] ++ ls]
    | otherwise -> do
        warn (groupId g) $ case groupKind g of
          Repeat -> "repeat written as plain questions, it is asked once"
          _ -> "nested group flattened into its block"
        when (isJust (groupTitle g)) $ warn (groupId g) "nested group title dropped"
        concat <$> mapM (writeItem (groupRelevant g `orE` rel)) (groupItems g)
  ItemRaw (Format f) _ -> [] <$ warn "qualtrics-txt" ("raw " <> f <> " content dropped")
  where
    textField = \case
      ItemQuestion q | qType q == QText ShortText -> Just (qLabel q)
      _ -> Nothing

orE :: Maybe Expr -> Maybe Expr -> Maybe Expr
orE (Just a) (Just b) = Just (EBin And a b)
orE a Nothing = a
orE Nothing b = b

writeQuestion :: Question -> Conv [Text]
writeQuestion q = do
  let tag = qId q
      selAttr = case M.lookup "qualtrics:selector" (qAttrs q) of
        Just (String s) -> Just s
        _ -> Nothing
      header t = ["[[Question:" <> t <> "]]", idLine tag, txt (qLabel q)]
      lossy what = warn tag what
  checkId tag
  checkText tag (qLabel q)
  when (qRequired q) $ lossy "required flag dropped"
  when (isJust (qRelevant q)) $ lossy "logic dropped"
  when (isJust (qHint q)) $ lossy "hint dropped"
  when (isJust (qValidation q)) $ lossy "validation dropped"
  when (isJust (qDefault q)) $ lossy "default value dropped"
  case qType q of
    QChoice sel style cs -> do
      when (style == Buttons) $ lossy "button style written as a plain choice list"
      let s = fromMaybe (selectorFor sel style) selAttr
          t = case (sel, s) of
            (Single, "DL") -> "MC:Dropdown"
            (Single, "SB") -> "MC:Select"
            (Single, "SAHR") -> "MC:SingleAnswer:Horizontal"
            (Single, _) -> "MC:SingleAnswer:Vertical"
            (Multiple, "MSB") -> "MC:MultiSelect"
            (Multiple, "MAHR") -> "MC:MultipleAnswer:Horizontal"
            (Multiple, _) -> "MC:MultipleAnswer:Vertical"
      (header t ++) <$> choiceLines tag "Choice" cs
    QBoolean -> do
      lossy "yes/no written as a single choice"
      (header "MC:SingleAnswer:Vertical" ++) <$> choiceLines tag "Choice" [choice "1" "Yes", choice "0" "No"]
    QScale lo hi l h -> do
      lossy ("scale written as a single choice" <> if isJust l || isJust h then ", end labels dropped" else "")
      -- a scale may run downwards (its low end above its high end)
      let points = if lo <= hi then [lo .. hi] else [lo, lo - 1 .. hi]
      when (scaleTooWide (min lo hi) (max lo hi)) $ lossy ("only the first " <> T.pack (show maxScalePoints) <> " points of the scale written")
      (header "MC:SingleAnswer:Horizontal" ++) <$> choiceLines tag "Choice" [choice (T.pack (show v)) (T.pack (show v)) | v <- take maxScalePoints points]
    QMatrix sel rows cols -> do
      unless (positional rows) $ lossy "row codes replaced by their position"
      rs <- choiceLines tag "Choice" [r {choiceValue = T.pack (show n)} | (n, r) <- zip [1 :: Int ..] rows]
      as <- choiceLines tag "Answer" cols
      pure (header (if sel == Multiple then "Matrix:MultipleAnswer" else "Matrix:SingleAnswer") ++ rs ++ as)
    QRanking cs -> (header "RO" ++) <$> choiceLines tag "Choice" cs
    QText k -> do
      unless (k `elem` [ShortText, LongText]) $ lossy "input check dropped, written as a text box"
      pure (header (if k == LongText then "TE:Essay" else "TE:SingleLine"))
    QNumber _ -> header "TE:SingleLine" <$ lossy "number check dropped, written as a text box"
    QDate _ -> header "TE:SingleLine" <$ lossy "date check dropped, written as a text box"
    QOther name -> case M.lookup "qualtrics:txt" (qAttrs q) of
      Just (String raw) -> pure (T.lines raw)
      _ -> descriptive ("unsupported type " <> name)
    QFile -> descriptive "file upload"
    QGeo -> descriptive "geopoint"
    QHidden -> pure []
    QCalculate _ -> pure []
  where
    descriptive what = do
      warn (qId q) (what <> " written as descriptive text")
      pure ["[[Question:Text]]", idLine (qId q), txt (qLabel q)]

-- | Text as the reader will see it: no trailing spaces, no blank lines at
-- either end, and no line that reads as a tag ('checkText' reports both).
-- The format has no escape, so a line such as @[[PageBreak]]@ inside a
-- label is written as @[ [PageBreak]]@: as written it would end the
-- question.
txt :: I18n -> Text
txt = T.intercalate "\n" . map untag . T.splitOn "\n" . block . T.lines . T.replace "\r" "\n" . T.replace "\r\n" "\n" . i18nDefault
  where
    untag l
      | isJust (tagOf l) = let (pad, rest) = T.span isSpace l in pad <> "[ " <> T.drop 1 rest
      | otherwise = l

-- | Warn when 'txt' changes respondent-facing text.
checkText :: Text -> I18n -> Conv ()
checkText at t = do
  let raw = i18nDefault t
      flat = T.replace "\r" "\n" (T.replace "\r\n" "\n" raw)
      ls = T.splitOn "\n" flat
  when (T.any (== '\r') raw) $ warn at "carriage returns written as line breaks"
  when (block ls /= flat) $ warn at "spaces at line ends and blank lines around text removed"
  forM_ [l | l <- ls, isJust (tagOf l)] $ \l ->
    warn at ("text line " <> T.strip l <> " reads as a tag, written with a space after the first [")

-- | An export tag as the reader will see it: one line, no edge spaces.
idLine :: Text -> Text
idLine i = "[[ID:" <> cleanId i <> "]]"

cleanId :: Text -> Text
cleanId = T.strip . T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

checkId :: Text -> Conv ()
checkId i = when (cleanId i /= i) $ warn i ("export tag written as " <> cleanId i)

positional :: [Choice] -> Bool
positional cs = and (zipWith (\n c -> T.pack (show n) == choiceValue c) [1 :: Int ..] cs)

-- | Choices as a plain list when codes are positions and labels fit on a
-- line, otherwise as advanced choices carrying their numeric recode.
choiceLines :: Text -> Text -> [Choice] -> Conv [Text]
choiceLines tag kind cs = do
  let numeric = all (isNat . choiceValue) cs
      simple = all (\c -> let l = txt (choiceLabel c) in not (T.null l) && l == T.strip l && not (T.any (== '\n') l)) cs
      plural = kind <> "s"
  mapM_ (checkText tag . choiceLabel) cs
  when (any choiceOther cs) $ warn tag "other-specify text box dropped"
  when (any choiceExclusive cs) $ warn tag "exclusive choice flag dropped"
  unless numeric $ warn tag "non-numeric choice codes replaced by their position"
  pure $
    if not numeric || positional cs
      then
        if simple
          then ("[[" <> plural <> "]]") : map (txt . choiceLabel) cs
          else ("[[Advanced" <> plural <> "]]") : concat [["[[" <> kind <> "]]", txt (choiceLabel c)] | c <- cs]
      else ("[[Advanced" <> plural <> "]]") : concat [["[[" <> kind <> ":" <> choiceValue c <> "]]", txt (choiceLabel c)] | c <- cs]
  where
    isNat t = case TR.decimal t :: Either String (Int, Text) of
      Right (_, "") -> True
      _ -> False
