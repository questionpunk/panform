-- | Forsta Surveys (Decipher) survey XML. Questions are elements named by
-- type (@radio@, @checkbox@, @select@, @text@, @textarea@, @number@,
-- @float@) with @<row>@, @<col>@ and @<choice>@ cells; @<suspend/>@ is a
-- page break, @<block>@ and @<loop>@ group, and @cond@ is a Python condition.
-- A radio or checkbox with both rows and columns is a grid.
module Panform.Format.Decipher (readDecipher, writeDecipher) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Panform.AST
import Panform.ChoiceLogic
import Panform.Conv
import Panform.Expr
import qualified Text.XML as X

------------------------------------------------------------------------------
-- Reading

readDecipher :: BL.ByteString -> Conv Form
readDecipher bs = do
  doc <- either (Left . show) Right (X.parseLBS X.def bs) `orFail` "Decipher XML"
  let root = X.documentRoot doc
  unless (local root == "survey") $ failWith "Decipher XML: root element is not <survey>"
  let hidden = [attr "label" e | e <- descendants root, local e == "text", attr "where" e == "execute"]
      calcs = M.fromList [c | e <- descendants root, Just c <- [calcOf e], fst c `elem` hidden]
  let taken = S.fromList [T.toLower (attr "label" e) | e <- descendants root, not (T.null (attr "label" e))]
  pieces <- concat <$> mapM (readNode taken calcs) (childElems root)
  metaAttrs' <- keptAttrs "survey" (M.delete "alt" (attrMap root))
  let meta =
        emptyMeta
          { metaTitle = plain <$> nonEmpty (attr "alt" root)
          , metaAttrs = M.map String metaAttrs'
          }
      s = Form meta (paginate taken "page" pieces)
      idx = choiceIndex False s
      codes = M.fromList [((qId q, l), v) | q <- questions s, Just (String m) <- [M.lookup "decipher:codes" (qAttrs q)], (l, v) <- decodeCodes m]
      value q n = M.findWithDefault n (q, n) codes
  pure (mapSurveyLogic (fromSelected idx value) (dropCodes s))

data Piece = PItem Item | PBreak

-- | Split at page breaks. A page that is exactly one block becomes that
-- block; other pages get a numbered id no label in the file uses; items
-- after the last break stay loose.
paginate :: S.Set Text -> Text -> [Piece] -> [Item]
paginate taken prefix ps
  | not (any isBreak ps) = [i | PItem i <- ps]
  | otherwise = go (1 :: Int) ps
  where
    isBreak = \case PBreak -> True; _ -> False
    go n xs = case break isBreak xs of
      (seg, _ : rest) -> let (is, n') = page n [i | PItem i <- seg] in is ++ go n' rest
      (seg, []) -> [i | PItem i <- seg]
    page n [] = ([], n + 1)
    page n [ItemGroup g] | groupKind g == Section = ([ItemGroup g {groupKind = Page}], n + 1)
    page n is =
      let m = until (\k -> S.notMember (T.toLower (name k)) taken) (+ 1) n
       in ([ItemGroup (Group (name m) Page Nothing Nothing is M.empty)], m + 1)
    name k = prefix <> T.pack (show k)

readNode :: S.Set Text -> M.Map Text Text -> X.Element -> Conv [Piece]
readNode taken calcs e = case local e of
  "suspend" -> pure [PBreak]
  "block" -> pure . PItem <$> readGroup taken calcs Section e
  "loop" -> pure . PItem <$> readGroup taken calcs Repeat e
  "html" -> do
    let i = attr "label" e
    pure [PItem (ItemNote i (plain (textOf e)) (cond e))]
  "exec" | isJust (calcOf e >>= \(q, _) -> M.lookup q calcs) -> pure []
  n
    | n `elem` ["radio", "checkbox", "select", "text", "textarea", "number", "float", "upload"] ->
        pure . PItem . ItemQuestion <$> readQuestion calcs e
    | otherwise -> pure [PItem (ItemRaw (Format "decipher") (String (renderEl e)))]

readGroup :: S.Set Text -> M.Map Text Text -> GroupKind -> X.Element -> Conv Item
readGroup taken calcs kind e = do
  let i = attr "label" e
      (rows, rest) = foldr (\c (a, b) -> if local c == "looprow" then (c : a, b) else (a, c : b)) ([], []) (childElems e)
  pieces <- concat <$> mapM (readNode taken calcs) rest
  extra <- keptAttrs i (foldr M.delete (attrMap e) ["label", "cond", "builder:title"])
  pure . ItemGroup $
    Group
      { groupId = i
      , groupKind = kind
      , groupTitle = plain <$> nonEmpty (attr "builder:title" e)
      , groupRelevant = cond e
      , groupItems = paginate taken (i <> "_p") pieces
      , groupAttrs =
          M.map String extra
            <> (if null rows then M.empty else M.singleton "decipher:looprows" (String (T.intercalate "\n" (map renderEl rows))))
      }

readQuestion :: M.Map Text Text -> X.Element -> Conv Question
readQuestion calcs e = do
  let i = attr "label" e
      kind = local e
      kids = childElems e
      title = maybe (attr "title" e) textOf (listToMaybe [c | c <- kids, local c == "title"])
      hint = listToMaybe [plain (textOf c) | c <- kids, local c == "comment"]
  rows <- mapM (cell i) [c | c <- kids, local c == "row"]
  cols <- mapM (cell i) [c | c <- kids, local c == "col"]
  chs <- mapM (cell i) [c | c <- kids, local c == "choice"]
  let a k = attr k e
      (rangeV, rangeOk) = case T.stripPrefix "range(" (a "verify") >>= T.stripSuffix ")" of
        Just args | [lo, hi] <- map (readD . T.strip) (T.splitOn "," args) -> (Just (Validation lo hi Nothing Nothing), True)
        _ -> (Nothing, False)
      scale = if a "type" == "rating" && null rows then asScale (map fst cols) else Nothing
      (qt, consumed) = case kind of
        "radio"
          | not (null rows), not (null cols) -> (QMatrix Single (map fst rows) (map fst cols), [])
          | Just s <- scale -> (s, ["type"])
          | null rows -> (QChoice Single Buttons (map fst cols), [])
          | otherwise -> (QChoice Single Radio (map fst rows), [])
        "checkbox"
          | not (null rows), not (null cols) -> (QMatrix Multiple (map fst rows) (map fst cols), [])
          | null rows -> (QChoice Multiple Buttons (map fst cols), [])
          | otherwise -> (QChoice Multiple Radio (map fst rows), [])
        "select"
          | not (null rows), a "unique" == "1" -> (QRanking (map fst rows), ["unique", "minRanks"])
          | not (null rows) -> (QMatrix Single (map fst rows) (map fst chs), [])
          | otherwise -> (QChoice Single Dropdown (map fst chs), [])
        "textarea" -> (QText LongText, [])
        "number" -> (QNumber Integer, ["verify" | rangeOk])
        "float" -> (QNumber Decimal, ["verify" | rangeOk])
        "upload" -> (QFile, [])
        _
          | a "where" == "execute", Just c <- M.lookup i calcs -> (QCalculate (parseOrRaw decipher c), ["where"])
          | a "where" == "execute" -> (QHidden, ["where"])
          | a "verify" == "email" -> (QText Email, ["verify"])
          | "phone" `T.isPrefixOf` a "verify" -> (QText Phone, ["verify"])
          | otherwise -> (QText ShortText, [])
      codes = [(l, v) | (_, (l, v)) <- rows ++ cols ++ chs, l /= v]
      cellsUsed
        | kind `elem` ["radio", "checkbox"] = if null rows then ["col"] else if null cols then ["row"] else ["row", "col"]
        | kind == "select" = if null rows then ["choice"] else ["row", "choice"]
        | otherwise = [] :: [Text]
  forM_ (nub [local c | c <- kids, local c `notElem` (["title", "comment"] ++ cellsUsed)]) $ \n ->
    warn i ("<" <> n <> "> inside the question dropped")
  extra <- keptAttrs i (foldr M.delete (attrMap e) (["label", "cond", "optional", "title"] ++ consumed))
  pure
    Question
      { qId = i
      , qLabel = plain title
      , qHint = hint
      , qType = qt
      , qRequired = a "optional" /= "1"
      , qRelevant = cond e
      , qValidation = if kind `elem` ["number", "float"] then rangeV else Nothing
      , qDefault = Nothing
      , qAttrs = M.map String extra <> (if null codes then M.empty else M.singleton "decipher:codes" (String (encodeCodes codes)))
      }
  where
    readD t = case reads (T.unpack t) :: [(Double, String)] of
      [(d, "")] -> Just d
      _ -> Nothing

-- | A row, column or choice: the choice and its (label, value) pair.
cell :: Text -> X.Element -> Conv (Choice, (Text, Text))
cell q c = do
  let l = attr "label" c
      v = fromMaybe l (nonEmpty (attr "value" c))
      known = ["label", "value", "open", "openSize", "exclusive", "randomize"]
  forM_ (M.keys (attrMap c)) $ \k ->
    unless (k `elem` known) $ warn q ("attribute " <> k <> " on " <> local c <> " " <> l <> " dropped")
  pure (Choice v (plain (textOf c)) (attr "open" c == "1") (attr "exclusive" c == "1"), (l, v))

-- | Row labels that differ from their values, kept while reading so that
-- conditions (which use labels) can be mapped back to values.
encodeCodes :: [(Text, Text)] -> Text
encodeCodes = T.intercalate " " . map (\(l, v) -> l <> "=" <> v)

decodeCodes :: Text -> [(Text, Text)]
decodeCodes = map (fmap (T.drop 1) . T.breakOn "=") . T.words

dropCodes :: Form -> Form
dropCodes (Form m items) = Form m (map go items)
  where
    go = \case
      ItemGroup g -> ItemGroup g {groupItems = map go (groupItems g)}
      ItemQuestion q -> ItemQuestion q {qAttrs = M.delete "decipher:codes" (qAttrs q)}
      i -> i

-- | @<exec>q.val = expr</exec>@, the way a hidden question is computed.
calcOf :: X.Element -> Maybe (Text, Text)
calcOf e
  | local e /= "exec" || not (M.null (attrMap e)) = Nothing
  | otherwise = case T.breakOn "=" (T.strip (textOf e)) of
      (lhs, rhs)
        | Just q <- T.stripSuffix ".val" (T.strip lhs)
        , isIdent q
        , Just r <- T.stripPrefix "=" rhs
        , not ("=" `T.isPrefixOf` r)
        , not (T.any (== '\n') r) ->
            Just (q, T.strip r)
      _ -> Nothing

cond :: X.Element -> Maybe Expr
cond e = parseOrRaw decipher <$> nonEmpty (attr "cond" e)

-- | Attributes worth keeping for a Decipher round trip; namespaced ones
-- outside the builder and ss namespaces are dropped with a warning.
keptAttrs :: Text -> M.Map Text Text -> Conv (M.Map Text Text)
keptAttrs at m = do
  let (bad, good) = M.partitionWithKey (\k _ -> T.any (== ':') k && not (any (`T.isPrefixOf` k) ["ss:", "builder:"])) m
  forM_ (M.keys bad) $ \k -> warn at ("attribute " <> k <> " dropped")
  pure (M.mapKeys ("decipher:" <>) good)

local :: X.Element -> Text
local = X.nameLocalName . X.elementName

-- | Attributes keyed @prefix:local@ for the builder and ss namespaces.
attrMap :: X.Element -> M.Map Text Text
attrMap e = M.fromList [(qname n, v) | (n, v) <- M.toList (X.elementAttributes e)]
  where
    qname n = case (X.nameNamespace n, X.namePrefix n) of
      (Just ns, _) | Just p <- lookup ns namespaces -> p <> ":" <> X.nameLocalName n
      (_, Just p) -> p <> ":" <> X.nameLocalName n
      _ -> X.nameLocalName n

namespaces :: [(Text, Text)]
namespaces = [("http://decipherinc.com/builder", "builder"), ("http://decipherinc.com/ss", "ss")]

attr :: Text -> X.Element -> Text
attr k e = M.findWithDefault "" k (attrMap e)

childElems :: X.Element -> [X.Element]
childElems e = [c | X.NodeElement c <- X.elementNodes e]

descendants :: X.Element -> [X.Element]
descendants e = concatMap (\c -> c : descendants c) (childElems e)

-- | Text content; @<br/>@ is a line break and @<alt>@ (report text) is
-- skipped.
textOf :: X.Element -> Text
textOf = T.strip . go
  where
    go e = T.concat (map node (X.elementNodes e))
    node = \case
      X.NodeContent t -> t
      X.NodeElement c
        | local c == "br" -> "\n"
        | local c == "alt" -> ""
        | otherwise -> go c
      _ -> ""

renderEl :: X.Element -> Text
renderEl e = stripDecl (TL.toStrict (X.renderText X.def (X.Document (X.Prologue [] Nothing []) e [])))
  where
    stripDecl t = case T.stripPrefix "<?xml" t of
      Just r -> T.drop 2 (snd (T.breakOn "?>" r))
      Nothing -> t

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.all isSpace t then Nothing else Just t

------------------------------------------------------------------------------
-- Writing

writeDecipher :: Form -> Conv BL.ByteString
writeDecipher s0@(Form meta _) = do
  s <- legalIds "q" pyName s0
  warnTranslations "decipher" s
  when (isJust (metaDescription meta)) $ warn "decipher" "survey description dropped"
  when (isJust (metaId meta)) $ warn "decipher" "survey id dropped"
  let kept = M.fromList [(k, v) | (k0, String v) <- M.toList (metaAttrs meta), Just k <- [T.stripPrefix "decipher:" k0]]
      rootAttrs =
        [("alt", maybe "" i18nDefault (metaTitle meta))]
          ++ [("xmlns:builder", "http://decipherinc.com/builder"), ("xmlns:ss", "http://decipherinc.com/ss")]
          ++ M.toList (if M.null kept then M.fromList [("name", "Survey"), ("state", "testing")] else kept)
      idx = choiceIndex True s
      prefix q = case qType q of
        QChoice Single Dropdown _ -> "ch"
        _ -> "r"
      labels = M.fromList [((qId q, choiceValue c), cellLabel (prefix q) c) | q <- questions s, (_, cs) <- [M.findWithDefault (Single, []) (qId q) idx], c <- cs]
      name q v = M.findWithDefault v (q, v) labels
      s' = mapSurveyLogic (toSelected idx name) s
  body <- concat <$> mapM (writeItem "page") (formItems s')
  let out = ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", open "survey" rootAttrs, ""] ++ body ++ ["</survey>"]
  pure (BL.fromStrict (TE.encodeUtf8 (T.unlines out)))

-- | Question labels are Python identifiers.
pyName :: Text -> Text
pyName v = case T.uncons clean of
  Just (c, _) | isAlpha c || c == '_' -> clean
  _ -> "q" <> clean
  where
    clean = T.map (\c -> if isAlphaNum c || c == '_' then c else '_') v

open :: Text -> [(Text, Text)] -> Text
open n as = "<" <> n <> T.concat [" " <> k <> "=\"" <> esc True v <> "\"" | (k, v) <- as] <> ">"

esc :: Bool -> Text -> Text
esc inAttr = T.concatMap $ \case
  '\r' -> "&#13;"
  '\n' | inAttr -> "&#10;"
  '\t' | inAttr -> "&#9;"
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' | inAttr -> "&quot;"
  c -> T.singleton c

leaf :: Text -> [(Text, Text)] -> Text -> Text
leaf n as body = T.dropEnd 1 (open n as) <> ">" <> esc False (T.strip body) <> "</" <> n <> ">"

indent :: [Text] -> [Text]
indent = map (\l -> if T.null l then l else "  " <> l)

condAttr :: Text -> Maybe Expr -> Conv [(Text, Text)]
condAttr at e = maybe [] (\c -> [("cond", c)]) <$> printLogic decipher at e

-- | Pages written without a block of their own: numbered ones with
-- nothing else to say, as the reader names them.
isPlainPage :: Text -> Group -> Bool
isPlainPage prefix g =
  groupKind g == Page
    && isNothing (groupTitle g)
    && isNothing (groupRelevant g)
    && M.null (groupAttrs g)
    && maybe False (\n -> not (T.null n) && T.all (`elem` ['0' .. '9']) n) (T.stripPrefix prefix (groupId g))

-- | Items inside a group whose auto-numbered pages have the given prefix.
writeItem :: Text -> Item -> Conv [Text]
writeItem prefix = \case
  ItemRaw (Format "decipher") (String t) -> pure (T.lines t)
  ItemRaw (Format f) _ -> [] <$ warn "decipher" ("raw " <> f <> " content dropped")
  ItemNote i t rel -> do
    c <- condAttr i rel
    pure [leaf "html" ([("label", i)] ++ c ++ [("where", "survey")]) (i18nDefault t)]
  ItemGroup g
    | isPlainPage prefix g, null (groupItems g) -> [] <$ warn (groupId g) "empty page dropped"
    | isPlainPage prefix g -> (++ ["<suspend/>"]) . concat <$> mapM (writeItem prefix) (groupItems g)
    | otherwise -> do
        c <- condAttr (groupId g) (groupRelevant g)
        kids <- concat <$> mapM (writeItem (groupId g <> "_p")) (groupItems g)
        let kept = [(k, v) | (k0, String v) <- M.toList (groupAttrs g), Just k <- [T.stripPrefix "decipher:" k0], k /= "looprows"]
            title = [("builder:title", i18nDefault t) | Just t <- [groupTitle g], not (T.all isSpace (i18nDefault t))]
            as = ("label", groupId g) : sortOn fst (c ++ title ++ kept)
        case groupKind g of
          Repeat -> do
            let looprows = [t | Just (String t) <- [M.lookup "decipher:looprows" (groupAttrs g)]]
            when (null looprows) $ warn (groupId g) "loop written without looprows; its iterations are unknown"
            pure ([open "loop" as] ++ indent (kids ++ concatMap T.lines looprows) ++ ["</loop>"])
          Page -> pure ([open "block" as] ++ indent kids ++ ["</block>", "<suspend/>"])
          Section -> pure ([open "block" as] ++ indent kids ++ ["</block>"])
  ItemQuestion q -> writeQuestion q

-- | A cell label for a choice: the value when it is an identifier, else
-- the prefix and the value (with @value=@ carrying an integer value).
cellLabel :: Text -> Choice -> Text
cellLabel prefix c = codeName prefix (choiceValue c)

writeQuestion :: Question -> Conv [Text]
writeQuestion q = do
  c <- condAttr (qId q) (qRelevant q)
  forM_ (qDefault q) $ \_ -> warn (qId q) "default value dropped"
  forM_ (qValidation q) $ \v -> do
    when (isJust (valExpr v)) $ warn (qId q) "validation expression dropped"
    when (isJust (valMessage v)) $ warn (qId q) "validation message dropped"
  let note = warn (qId q)
      kept = M.fromList [(k, v) | (k0, String v) <- M.toList (qAttrs q), Just k <- [T.stripPrefix "decipher:" k0]]
      base = c ++ [("optional", "1") | not (qRequired q)]
      cells n prefix cs = forM cs $ \ch -> do
        let l = cellLabel prefix ch
            v = choiceValue ch
        unless (l == v || isInteger v) $ note ("choice value " <> v <> " written as label " <> l)
        pure $
          leaf
            n
            ( [("label", l)]
                ++ [("value", v) | l /= v, isInteger v]
                ++ [("exclusive", "1") | choiceExclusive ch]
                ++ [("open", "1") | choiceOther ch]
                ++ [("openSize", "25") | choiceOther ch]
                ++ [("randomize", "0") | choiceOther ch || choiceExclusive ch]
            )
            (i18nDefault (choiceLabel ch))
      range v = case (valMin v, valMax v) of
        (Just lo, Just hi) -> pure [("verify", "range(" <> num lo <> "," <> num hi <> ")")]
        (Nothing, Nothing) -> pure []
        _ -> [] <$ note "one-sided range dropped"
      el n extra body = do
        let as = ("label", qId q) : sortOn fst (M.toList (M.fromList (base ++ extra) `M.union` kept))
        pure ([open n as] ++ indent ([leaf "title" [] (i18nDefault (qLabel q)) | not (T.all isSpace (i18nDefault (qLabel q)))] ++ [leaf "comment" [] (i18nDefault h) | Just h <- [qHint q]] ++ body) ++ ["</" <> n <> ">"])
  case qType q of
    QChoice sel style cs -> do
      let n = if sel == Single then "radio" else "checkbox"
      case style of
        Dropdown | sel == Single -> cells "choice" "ch" cs >>= el "select" []
        Buttons -> cells "col" "r" cs >>= el n []
        _ -> do
          when (style == Dropdown) $ note "multi-select dropdown written as checkboxes"
          cells "row" "r" cs >>= el n []
    QMatrix sel rs cs -> do
      rows <- cells "row" "r" rs
      cols <- cells "col" "c" cs
      el (if sel == Single then "radio" else "checkbox") [] (rows ++ cols)
    QScale lo hi l h -> cells "col" "c" (scaleChoices lo hi l h) >>= el "radio" [("type", "rating")]
    QRanking cs -> do
      rows <- cells "row" "r" cs
      ranks <- cells "choice" "ch" [choice (T.pack (show k)) (T.pack (show k)) | k <- [1 .. length cs]]
      el "select" [("unique", "1"), ("minRanks", T.pack (show (length cs)))] (rows ++ ranks)
    QBoolean -> do
      note "boolean written as a yes/no radio"
      cells "row" "r" boolChoices >>= el "radio" []
    QText k -> case k of
      LongText -> el "textarea" [] []
      Email -> el "text" [("verify", "email")] []
      Phone -> note "phone validation written as phoneUS" >> el "text" [("verify", "phoneUS")] []
      Url -> note "url input written as plain text" >> el "text" [] []
      ShortText -> el "text" [] []
    QNumber k -> do
      r <- maybe (pure []) range (qValidation q)
      el (if k == Integer then "number" else "float") r []
    QDate _ -> note "date written as a text box" >> el "text" [] []
    QFile -> el "upload" [] []
    QGeo -> note "geopoint written as a text box" >> el "text" [] []
    QHidden -> el "text" [("where", "execute")] []
    QCalculate e -> do
      rhs <- printLogic decipher (qId q) (Just e)
      t <- el "text" [("where", "execute")] []
      pure (t ++ ["<exec>" <> esc False (qId q) <> ".val = " <> esc False r <> "</exec>" | Just r <- [rhs]])
    QOther t -> note ("type " <> t <> " written as a text box") >> el "text" [] []
  where
    num x = if x == fromIntegral (round x :: Integer) then T.pack (show (round x :: Integer)) else T.pack (show x)
