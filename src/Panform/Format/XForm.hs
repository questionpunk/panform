-- | ODK XForms: the XML that pyxform compiles XLSForm into and that ODK
-- Collect, Enketo and KoboToolbox serve. The primary @instance@ gives the
-- question tree and order, @bind@s carry types and XPath logic, body
-- controls carry labels and choices, and @itext@ carries translations
-- referenced as @jr:itext('id')@. Written the way pyxform writes it.
module Panform.Format.XForm (readXForm, writeXForm) where

import Control.Monad (forM, mfilter, unless, void, when)
import Control.Monad.State.Strict (StateT, get, gets, lift, modify', put, runStateT)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub, sortOn, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Expr
import qualified Text.XML as X

------------------------------------------------------------------------------
-- XML helpers (namespace-agnostic on read)

local :: X.Element -> Text
local = X.nameLocalName . X.elementName

kids :: X.Element -> [X.Element]
kids e = [c | X.NodeElement c <- X.elementNodes e]

kidsNamed :: Text -> X.Element -> [X.Element]
kidsNamed n = filter ((== n) . local) . kids

kid :: Text -> X.Element -> Maybe X.Element
kid n = listToMaybe . kidsNamed n

descendantsNamed :: Text -> X.Element -> [X.Element]
descendantsNamed n e = concat [[c | local c == n] ++ descendantsNamed n c | c <- kids e]

attr :: Text -> X.Element -> Maybe Text
attr n e = listToMaybe [v | (k, v) <- M.toList (X.elementAttributes e), X.nameLocalName k == n]

attrOr :: Text -> X.Element -> Text
attrOr n = fromMaybe "" . attr n

-- | Attributes other than the listed local names, keyed by their prefixed
-- name (@jr:count@) so a writer can emit them again.
extraAttrs :: [Text] -> X.Element -> [(Text, Text)]
extraAttrs known e =
  [ (maybe "" (<> ":") (X.namePrefix k) <> X.nameLocalName k, v)
  | (k, v) <- M.toList (X.elementAttributes e)
  , X.nameLocalName k `notElem` known
  ]

textOf :: X.Element -> Text
textOf e = T.concat [t | X.NodeContent t <- X.elementNodes e]

-- | Label text; an @<output value="/data/q"/>@ becomes @${q}@.
richText :: X.Element -> Text
richText e = T.concat (map node (X.elementNodes e))
  where
    node (X.NodeContent t) = t
    node (X.NodeElement c)
      | local c == "output" = "${" <> lastStep (T.strip (attrOr "value" c)) <> "}"
      | otherwise = richText c
    node _ = ""

lastStep :: Text -> Text
lastStep = T.strip . snd . T.breakOnEnd "/"

-- | A minimal XML tree for writing; rendered with two-space indentation
-- and attributes sorted by name, as pyxform does.
data X = E Text [(Text, Text)] [X] | T Text

render :: X -> Text
render = go 0
  where
    go :: Int -> X -> Text
    go n (T t) = ind n <> escText t <> "\n"
    go n (E name as cs)
      | null cs = ind n <> open name as <> "/>\n"
      | any isText cs = ind n <> open name as <> ">" <> T.concat (map inline cs) <> "</" <> name <> ">\n"
      | otherwise = ind n <> open name as <> ">\n" <> T.concat (map (go (n + 1)) cs) <> ind n <> "</" <> name <> ">\n"
    inline (T t) = escText t
    inline (E name as []) = open name as <> "/>"
    inline (E name as cs) = open name as <> ">" <> T.concat (map inline cs) <> "</" <> name <> ">"
    open name as = "<" <> name <> T.concat [" " <> k <> "=\"" <> escAttr v <> "\"" | (k, v) <- sortOn fst as]
    ind n = T.replicate n "  "
    isText (T _) = True
    isText _ = False
    -- A parser normalises line ends in text, and all whitespace in
    -- attributes, so those are written as character references.
    escText = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "\r" "&#13;" . T.replace "&" "&amp;"
    escAttr = T.replace "\t" "&#9;" . T.replace "\n" "&#10;" . T.replace "\"" "&quot;" . escText

------------------------------------------------------------------------------
-- Reading

data Ctx = Ctx
  { cBinds :: M.Map Text X.Element
  , cControls :: M.Map Text X.Element
  -- ^ Body controls and groups by the path they reference.
  , cRepeats :: M.Map Text X.Element
  , cItext :: M.Map Text I18n
  , cInstances :: M.Map Text X.Element
  -- ^ Secondary instances by id.
  }

readXForm :: BL.ByteString -> Conv Form
readXForm bs = do
  doc <- X.parseLBS X.def bs `orFail'` "XForms XML"
  let root = X.documentRoot doc
      hd = kid "head" root
      model = hd >>= kid "model"
      body = fromMaybe (X.Element "body" M.empty []) (kid "body" root)
  m <- maybe (failWith "XForms has no h:head/model") pure model
  let instances = kidsNamed "instance" m
      primary = listToMaybe [i | i <- instances, isNothing (attr "id" i)] >>= listToMaybe . kids
      secondary = M.fromList [(i', i) | i <- instances, Just i' <- [attr "id" i]]
  dataEl <- maybe (failWith "XForms has no primary instance") pure primary
  (itext, langs) <- readItext m
  let binds = M.fromList [(T.strip n, b) | b <- descendantsNamed "bind" m, Just n <- [attr "nodeset" b]]
      bindIds = M.fromList [(i, T.strip n) | b <- descendantsNamed "bind" m, Just i <- [attr "id" b], Just n <- [attr "nodeset" b]]
      refOf c = case attr "ref" c of
        Just r -> Just (T.strip r)
        Nothing -> attr "bind" c >>= (`M.lookup` bindIds)
      allControls = descendantsNamed' body
      controls = M.fromList [(r, c) | c <- allControls, local c /= "repeat", Just r <- [refOf c]]
      repeats = M.fromList [(T.strip r, c) | c <- allControls, local c == "repeat", Just r <- [attr "nodeset" c]]
      ctx = Ctx binds controls repeats itext secondary
      rootName = local dataEl
  unless (null (kidsNamed "submission" m)) $ warn "xform" "submission settings dropped"
  items <- readChildren ctx ("/" <> rootName) dataEl
  let meta =
        emptyMeta
          { metaId = attr "id" dataEl
          , -- A form with no title gets its id as the title, as in pyxform.
            metaTitle = plain <$> mfilter (\t -> Just t /= attr "id" dataEl && not (T.null t)) (textOf <$> (hd >>= kid "title"))
          , metaLanguages = langs
          , metaAttrs =
              M.fromList $
                [("xform:root", String rootName) | rootName /= "data"]
                  ++ [("xform:version", String v) | Just v <- [attr "version" dataEl]]
          }
  pure (Form meta items)
  where
    orFail' e ctx = either (Left . show) Right e `orFail` ctx
    descendantsNamed' e = concat [c : descendantsNamed' c | c <- kids e]

-- | The itext table, merged per text id; the default translation (or the
-- first) supplies the default text. A value of @-@ means "not translated".
readItext :: X.Element -> Conv (M.Map Text I18n, [Lang])
readItext m = case kid "itext" m of
  Nothing -> pure (M.empty, [])
  Just it -> do
    let trs = kidsNamed "translation" it
        isDef t = attr "default" t `elem` [Just "true()", Just "true"]
        ordered = filter isDef trs ++ filter (not . isDef) trs
        texts isDefault t =
          M.fromList
            [ (i, v)
            | x <- kidsNamed "text" t
            , Just i <- [attr "id" x]
            , let vs = [richText val | val <- kidsNamed "value" x, isNothing (attr "form" val)]
            , Just v <- [listToMaybe vs]
            , isDefault || T.strip v /= "-"
            ]
        media = S.fromList [i | t <- trs, x <- kidsNamed "text" t, any (isJust . attr "form") (kidsNamed "value" x), Just i <- [attr "id" x]]
    unless (S.null media) $ warn "xform" ("media labels dropped for " <> T.pack (show (S.size media)) <> " text ids")
    case ordered of
      [] -> pure (M.empty, [])
      (d : rest) -> do
        let defTexts = texts True d
            others = [(langCode (attrOr "lang" t), texts False t) | t <- rest]
            ids = S.toList (S.unions (M.keysSet defTexts : map (M.keysSet . snd) others))
            entry i = case M.lookup i defTexts of
              Just v -> Just (I18n v (M.fromList [(l, t) | (l, ts) <- others, Just t <- [M.lookup i ts]]))
              Nothing -> case [(l, t) | (l, ts) <- others, Just t <- [M.lookup i ts]] of
                ((_, t) : more) -> Just (I18n t (M.fromList more))
                [] -> Nothing
            defLang = langCode (attrOr "lang" d)
            langs = if defLang == "default" then [] else defLang : map fst others
        pure (M.fromList [(i, e) | i <- ids, Just e <- [entry i]], langs)

-- | "English (en)" -> "en"; anything else is used as the tag itself.
langCode :: Text -> Lang
langCode l = case T.breakOnEnd "(" l of
  (pre, code) | not (T.null pre), ")" `T.isSuffixOf` code -> T.dropEnd 1 code
  _ -> l

-- | Children of an instance node, in instance order. A repeat's template
-- and its sample copies share a name, so only the first is read.
readChildren :: Ctx -> Text -> X.Element -> Conv [Item]
readChildren ctx path el = do
  let named = dedupe (kids el)
      isRoot = T.count "/" path == 1
  metaExtra <- pure [n | isRoot, m <- named, local m == "meta", n <- kids m, local n `notElem` ["instanceID", "deprecatedID"]]
  unless (null metaExtra) $ warn "xform" ("meta fields dropped: " <> T.intercalate ", " (map local metaExtra))
  catMaybes <$> mapM (\c -> readNode ctx (path <> "/" <> local c) c) [c | c <- named, not (isRoot && local c == "meta")]
  where
    dedupe = go S.empty
      where
        go _ [] = []
        go seen (c : cs)
          | local c `S.member` seen = go seen cs
          | otherwise = c : go (S.insert (local c) seen) cs

readNode :: Ctx -> Text -> X.Element -> Conv (Maybe Item)
readNode ctx path el
  | not (null (kids el)) || isGroupControl = Just <$> readGroup ctx path el
  | otherwise = Just <$> readLeaf ctx path el
  where
    isGroupControl = maybe False ((== "group") . local) (M.lookup path (cControls ctx)) || M.member path (cRepeats ctx)

readGroup :: Ctx -> Text -> X.Element -> Conv Item
readGroup ctx path el = do
  children <- readChildren ctx path el
  let ctl = M.lookup path (cControls ctx)
      rep = M.lookup path (cRepeats ctx)
      labelEl = (ctl >>= kid "label") `orElse` (rep >>= kid "label")
      ws = maybe [] (T.words . attrOr "appearance") ctl
      kind
        | isJust rep = Repeat
        | "field-list" `elem` ws = Page
        | otherwise = Section
      bind = M.lookup path (cBinds ctx)
      extras = (ws \\ ["field-list"])
      attrs =
        M.fromList $
          [("xform:appearance", String (T.unwords extras)) | not (null extras)]
            ++ [("xform:body:" <> k, String v) | Just c <- [ctl], (k, v) <- extraAttrs ["ref", "appearance"] c]
            ++ [("xform:repeat:" <> k, String v) | Just r <- [rep], (k, v) <- extraAttrs ["nodeset", "appearance"] r]
            ++ [("xform:bind:" <> k, String v) | Just b <- [bind], (k, v) <- extraAttrs ["nodeset", "relevant", "type"] b]
      g =
        Group
          { groupId = local el
          , groupKind = kind
          , groupTitle = labelEl >>= labelText ctx
          , groupRelevant = relevance bind
          , groupItems = children
          , groupAttrs = attrs
          }
  pure (asMatrix g)
  where
    orElse a b = maybe b Just a

relevance :: Maybe X.Element -> Maybe Expr
relevance b = parseOrRaw (xpath id) <$> (b >>= attr "relevant")

labelText :: Ctx -> X.Element -> Maybe I18n
labelText ctx l = case attr "ref" l of
  Just r | Just i <- itextId r -> M.lookup i (cItext ctx)
  _ -> Just (plain (richText l))

-- | @jr:itext('id')@ -> @id@.
itextId :: Text -> Maybe Text
itextId r = do
  inner <- T.stripPrefix "jr:itext(" (T.strip r) >>= T.stripSuffix ")"
  pure (T.dropAround (`elem` ("'\" " :: String)) inner)

-- | The pyxform table-list idiom, as the writer emits it: a field-list
-- group of selects named @<group>_<row>@ sharing one choice list, each row
-- @list-nolabel@ with nothing else of its own. A group whose rows carry
-- logic, a hint, a default, validation or differing required flags stays
-- a group, because a matrix has no place for them.
asMatrix :: Group -> Item
asMatrix g = case groupItems g of
  items@(_ : _)
    | groupKind g == Page
    , M.null (groupAttrs g)
    , Just rs <- mapM row items
    , ((sel, cols, _, q0) : _) <- rs
    , all (\(s, c, _, q) -> s == sel && c == cols && qRequired q == qRequired q0) rs
    , (header, body) <- span (\(_, _, a, _) -> a == "label") rs
    -- A header row is a question of its own in the data, so a group
    -- with one stays a group.
    , null header
    , not (null body)
    , all (\(_, _, a, _) -> a == "list-nolabel") body ->
        ItemQuestion
          (question (groupId g) "" (QMatrix sel [Choice v (qLabel q) False False | (_, _, _, q) <- body, Just v <- [T.stripPrefix prefix (qId q)]] cols))
            { qLabel = fromMaybe (plain "") (groupTitle g)
            , qRelevant = groupRelevant g
            , qRequired = qRequired q0
            , qAttrs = M.fromList [(k, v) | k <- ["xform:list", "xform:items"], Just v <- [M.lookup k (qAttrs q0)]]
            }
  _ -> ItemGroup g
  where
    prefix = groupId g <> "_"
    row (ItemQuestion q)
      | QChoice sel _ cs <- qType q
      , Just (String a) <- M.lookup "xform:appearance" (qAttrs q)
      , all (`elem` ["xform:appearance", "xform:list", "xform:items"]) (M.keys (qAttrs q))
      , prefix `T.isPrefixOf` qId q
      , isNothing (qRelevant q) && isNothing (qHint q) && isNothing (qValidation q) && isNothing (qDefault q) =
          Just (sel, cs, a, q)
    row _ = Nothing

readLeaf :: Ctx -> Text -> X.Element -> Conv Item
readLeaf ctx path el = do
  let name = local el
      bind = M.lookup path (cBinds ctx)
      ctl = M.lookup path (cControls ctx)
      battr k = bind >>= attr k
      bindType = maybe "string" (\t -> fromMaybe t (T.stripPrefix "xsd:" t)) (battr "type")
      ws = maybe [] (T.words . attrOr "appearance") ctl
      label = fromMaybe (plain "") (ctl >>= kid "label" >>= labelText ctx)
      hint = ctl >>= kid "hint" >>= labelText ctx
      readonly = battr "readonly" `elem` [Just "true()", Just "true"]
      isNote = maybe False ((== "input") . local) ctl && readonly && isNothing (battr "calculate") && bindType == "string"
  (qt, consumed) <- case (battr "calculate", ctl) of
    (Just c, _) -> do
      when (isJust ctl) $ warn name "displayed calculation read as a hidden calculation"
      pure (QCalculate (parseOrRaw (xpath id) c), [])
    (Nothing, Nothing) -> pure (QHidden, [])
    (Nothing, Just c) -> controlType ctx name bindType ws c
  let validation = readValidation ctx qt bind
      extrasW = ws \\ consumed
      derivedType = bindTypeFor qt
      -- A required expression is not a yes or a no: the question is not
      -- always required, and the attribute is kept as it is.
      requiredFlag = maybe True (`elem` ["true()", "true", "false()", "false", ""]) (T.strip <$> battr "required")
      bindKnown = ["nodeset", "type", "relevant", "constraint", "constraintMsg", "calculate"] ++ ["required" | requiredFlag] ++ ["readonly" | isNote]
      attrs =
        M.fromList $
          [("xform:appearance", String (T.unwords extrasW)) | not (null extrasW)]
            ++ [("xform:control", "trigger") | Just c <- [ctl], local c == "trigger"]
            ++ [("xform:list", String l) | Just c <- [ctl], Just is <- [kid "itemset" c], let l = itemsetInstance (attrOr "nodeset" is), not (T.null l)]
            ++ [("xform:items", "inline") | Just c <- [ctl], local c `elem` ["select1", "select", "rank"], isNothing (kid "itemset" c), not (null (kidsNamed "item" c))]
            ++ [("xform:bind:type", String t) | Just t <- [battr "type"], bindType /= derivedType, not isNote]
            ++ [("xform:body:" <> k, String v) | Just c <- [ctl], (k, v) <- extraAttrs ["ref", "appearance", "start", "end"] c]
            ++ [("xform:bind:" <> k, String v) | Just b <- [bind], (k, v) <- extraAttrs bindKnown b]
  when (not isNote && isNothing ctl && not (T.null (i18nDefault label))) $ warn name "label of a hidden field dropped"
  pure $
    if isNote
      then ItemNote name label (relevance bind)
      else
        ItemQuestion
          Question
            { qId = name
            , qLabel = label
            , qHint = hint
            , qType = qt
            , qRequired = (T.strip <$> battr "required") `elem` [Just "true()", Just "true"]
            , qRelevant = relevance bind
            , qValidation = validation
            , qDefault = let t = T.strip (textOf el) in if T.null t then Nothing else Just t
            , qAttrs = attrs
            }

-- | @instance('list')/root/item[...]@ -> @list@.
itemsetInstance :: Text -> Text
itemsetInstance ns = T.takeWhile (`notElem` ("'\")" :: String)) (T.drop 1 (T.dropWhile (`notElem` ("'\"" :: String)) (fst (T.breakOn "[" ns))))

-- | The type a body control and bind type describe, and the appearance
-- words that type accounts for (a writer emits them again).
controlType :: Ctx -> Text -> Text -> [Text] -> X.Element -> Conv (QType, [Text])
controlType ctx name bindType ws c = case local c of
  "input" -> pure $ case bindType of
    "int" -> (QNumber Integer, [])
    "decimal" -> (QNumber Decimal, [])
    "date" -> (QDate Date, [])
    "time" -> (QDate Time, [])
    "dateTime" -> (QDate DateTime, [])
    "geopoint" -> (QGeo, [])
    "geotrace" -> (QGeo, [])
    "geoshape" -> (QGeo, [])
    "string"
      | "multiline" `elem` ws -> (QText LongText, ["multiline"])
      | "numbers" `elem` ws -> (QText Phone, ["numbers"])
      | otherwise -> (QText ShortText, [])
    t -> (QOther t, [])
  "select1" -> (\cs -> (QChoice Single style cs, styleWords)) <$> choices
  "select" -> (\cs -> (QChoice Multiple style cs, styleWords)) <$> choices
  "rank" -> (\cs -> (QRanking cs, [])) <$> choices
  "range" -> pure (QScale (num "start" 1) (num "end" 10) Nothing Nothing, [])
  "upload" -> pure (QFile, [])
  "trigger" -> pure (QBoolean, [])
  other -> (QOther other, []) <$ warn name ("unknown control " <> other <> " kept by name")
  where
    style
      | any (`elem` ws) ["minimal", "autocomplete"] = Dropdown
      | any (`elem` ws) ["quick", "likert"] = Buttons
      | otherwise = Radio
    styleWords = filter (`elem` ["minimal", "quick"]) ws
    num k d = maybe d round (attr k c >>= readD)
    readD t = case reads (T.unpack t) :: [(Double, String)] of
      [(x, "")] -> Just x
      _ -> Nothing
    choices = case kid "itemset" c of
      Nothing -> pure [Choice (T.strip (maybe "" textOf (kid "value" i))) (fromMaybe (plain "") (kid "label" i >>= labelText ctx)) False False | i <- kidsNamed "item" c]
      Just is -> itemset is
    itemset is = do
      let ns = attrOr "nodeset" is
          (src, rest) = T.breakOn "[" ns
          instId = itemsetInstance src
          valueRef = maybe "name" (attrOr "ref") (kid "value" is)
          labelRef = maybe "label" (attrOr "ref") (kid "label" is)
      unless (T.null rest) $ warn name ("choice filter dropped: " <> rest)
      case M.lookup instId (cInstances ctx) of
        Nothing -> [] <$ warn name ("choice instance " <> instId <> " not found")
        Just inst -> do
          let items = concatMap kids (kids inst)
              field f i = maybe "" (T.strip . textOf) (kid f i)
              lbl i = case itextId labelRef of
                Just f -> fromMaybe (plain "") (M.lookup (field f i) (cItext ctx))
                Nothing -> plain (maybe "" richText (kid labelRef i))
          pure [Choice (field valueRef i) (lbl i) False False | i <- items]

readValidation :: Ctx -> QType -> Maybe X.Element -> Maybe Validation
readValidation ctx qt b = do
  c <- b >>= attr "constraint"
  let msg = b >>= attr "constraintMsg" >>= \m -> case itextId m of
        Just i -> M.lookup i (cItext ctx)
        Nothing -> Just (plain m)
      (lo, hi, rest) = splitBounds qt (parseOrRaw (xpath id) c)
  pure (Validation lo hi rest msg)

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
      EBin And a z -> spine a ++ [z]
      x -> [x]
    go Nothing hi (EBin Ge s (ELit (LNum n)) : r) | s == subject = go (Just n) hi r
    go lo Nothing (EBin Le s (ELit (LNum n)) : r) | s == subject = go lo (Just n) r
    go lo hi r = (lo, hi, case r of [] -> Nothing; (x : xs) -> Just (foldl (EBin And) x xs))

------------------------------------------------------------------------------
-- Writing

-- | The bind type a writer gives a question type.
bindTypeFor :: QType -> Text
bindTypeFor = \case
  QNumber Integer -> "int"
  QNumber Decimal -> "decimal"
  QDate Date -> "date"
  QDate Time -> "time"
  QDate DateTime -> "dateTime"
  QGeo -> "geopoint"
  QFile -> "binary"
  QScale {} -> "int"
  QRanking _ -> "odk:rank"
  QOther t | t `elem` knownBindTypes -> t
  _ -> "string"

knownBindTypes :: [Text]
knownBindTypes = ["barcode", "geotrace", "geoshape", "binary", "intent"]

data WS = WS
  { wsLists :: [(Text, [Choice])]
  , wsItext :: [(Text, I18n)]
  }

type W = StateT WS Conv

data Env = Env
  { eMulti :: Bool
  , ePath :: Text -> Text
  , eBools :: M.Map Text Bool
  -- ^ Boolean questions, 'True' for an acknowledge trigger.
  }

writeXForm :: Form -> Conv BL.ByteString
writeXForm (Form meta items0) = do
  items <- uniqueNames items0
  let s = Form meta items
  let rootName = case M.lookup "xform:root" (metaAttrs meta) of
        Just (String r) -> r
        _ -> "data"
      pm = M.fromList (paths ("/" <> rootName) items)
      defLang = fromMaybe "default" (listToMaybe (metaLanguages meta))
      langs = nub (concatMap i18nLangs (surveyTexts s)) \\ [defLang]
      bools = M.fromList [(qId q, M.lookup "xform:control" (qAttrs q) == Just "trigger") | q <- questions s, qType q == QBoolean]
      -- itext also carries a named default language when it is the only one.
      multi = not (null langs) || not (null (metaLanguages meta)) && defLang /= "default"
      env = Env multi (\q -> M.findWithDefault ("/" <> rootName <> "/" <> xmlName q) q pm) bools
  when (any (elem "-" . M.elems . i18nTranslations) (surveyTexts s)) $ warn "xform" "a translation that is just \"-\" reads back as untranslated"
  case metaTitle meta of
    Just t | not (M.null (i18nTranslations t)) -> warn "title" "title translations dropped"
    _ -> pure ()
  when (isJust (metaDescription meta)) $ warn "description" "survey description dropped"
  ((inst, binds, body), st) <- runStateT (writeItems env ("/" <> rootName) items) (WS [] [])
  let formId = fromMaybe rootName (metaId meta)
      version = [("version", v) | Just (String v) <- [M.lookup "xform:version" (metaAttrs meta)]]
      itext
        | eMulti env =
            [ E "itext" [] $
                [ E "translation" ([("lang", l)] ++ [("default", "true()") | l == defLang]) $
                    [ E "text" [("id", i)] [E "value" [] (richNodes env (if l == defLang then i18nDefault t else M.findWithDefault "-" l (i18nTranslations t)))]
                    | (i, t) <- reverse (wsItext st)
                    ]
                | l <- defLang : langs
                ]
            ]
        | otherwise = []
      secondary =
        [ E "instance" [("id", n)] [E "root" [] (zipWith (choiceItem env n) [0 :: Int ..] cs)]
        | (n, cs) <- wsLists st
        ]
      metaNode = E "meta" [] [E "instanceID" [] []]
      metaBind = E "bind" [("nodeset", "/" <> rootName <> "/meta/instanceID"), ("type", "string"), ("readonly", "true()"), ("jr:preload", "uid")] []
      html =
        E
          "h:html"
          [ ("xmlns", "http://www.w3.org/2002/xforms")
          , ("xmlns:ev", "http://www.w3.org/2001/xml-events")
          , ("xmlns:h", "http://www.w3.org/1999/xhtml")
          , ("xmlns:jr", "http://openrosa.org/javarosa")
          , ("xmlns:odk", "http://www.opendatakit.org/xforms")
          , ("xmlns:orx", "http://openrosa.org/xforms")
          , ("xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
          ]
          [ E
              "h:head"
              []
              [ E "h:title" [] [T (maybe formId i18nDefault (metaTitle meta))]
              , E "model" [("odk:xforms-version", "1.0.0")] $
                  itext
                    ++ [E "instance" [] [E rootName ([("id", formId)] ++ version) (inst ++ [metaNode])]]
                    ++ secondary
                    ++ binds
                    ++ [metaBind]
              ]
          , E "h:body" [] body
          ]
  pure (BL.fromStrict (TE.encodeUtf8 ("<?xml version=\"1.0\"?>\n" <> render html)))

choiceItem :: Env -> Text -> Int -> Choice -> X
choiceItem env list n c
  | eMulti env = E "item" [] [E "itextId" [] [T (list <> "-" <> T.pack (show n))], E "name" [] [T (choiceValue c)]]
  | otherwise = E "item" [] [E "label" [] [T (i18nDefault (choiceLabel c))], E "name" [] [T (choiceValue c)]]

-- | Every translatable text, to find the languages in use.
surveyTexts :: Form -> [I18n]
surveyTexts (Form _ items) = concatMap go (allItems items)
  where
    go = \case
      ItemGroup g -> maybe [] pure (groupTitle g)
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q ->
        [qLabel q] ++ maybe [] pure (qHint q) ++ maybe [] pure (qValidation q >>= valMessage) ++ case qType q of
          QChoice _ _ cs -> map choiceLabel cs
          QRanking cs -> map choiceLabel cs
          QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
          _ -> []

-- | Each question and group's absolute node path, for printing logic.
paths :: Text -> [Item] -> [(Text, Text)]
paths prefix = concatMap $ \case
  ItemGroup g -> let p = prefix <> "/" <> xmlName (groupId g) in (groupId g, p) : paths p (groupItems g)
  ItemQuestion q -> [(qId q, prefix <> "/" <> xmlName (qId q))]
  ItemNote i _ _ -> [(i, prefix <> "/" <> xmlName i)]
  ItemRaw _ _ -> []

-- | Label text with @${q}@ turned into @<output>@ elements.
richNodes :: Env -> Text -> [X]
richNodes env t = case T.breakOn "${" t of
  (pre, "") -> [T pre]
  (pre, rest) -> case T.breakOn "}" (T.drop 2 rest) of
    (q, after) | not (T.null after) -> [T pre | not (T.null pre)] ++ [E "output" [("value", ePath env q)] []] ++ richNodes env (T.drop 1 after)
    _ -> [T t]

-- | A label: an itext reference when the form is multilingual.
labelX :: Env -> Text -> Text -> I18n -> W X
labelX env tag i t
  | eMulti env = do
      modify' (\w -> w {wsItext = (i, t) : wsItext w})
      pure (E tag [("ref", "jr:itext('" <> i <> "')")] [])
  | otherwise = pure (E tag [] (richNodes env (i18nDefault t)))

logic :: Env -> Text -> Maybe Expr -> W (Maybe Text)
logic env at e = case [f | Just x <- [e], f <- callsIn x, f `notElem` odkFunctions] of
  (f : _) -> Nothing <$ lift (warn at ("logic dropped, ODK XPath has no function " <> f))
  [] -> lift (printLogic (xpath (ePath env)) at (toXPath env <$> e))

-- | Functions an expression calls, which ODK must know or the form fails
-- to load.
callsIn :: Expr -> [Text]
callsIn = \case
  ECall f xs -> f : concatMap callsIn xs
  ENot x -> callsIn x
  EBin _ a b -> callsIn a ++ callsIn b
  _ -> []

-- | The XPath 1.0 and ODK XForms functions (getodk.github.io/xforms-spec).
odkFunctions :: [Text]
odkFunctions =
  [ "true", "false", "not", "boolean", "number", "string", "concat", "join", "substr", "substring-before", "substring-after"
  , "translate", "string-length", "normalize-space", "contains", "starts-with", "ends-with", "selected", "selected-at"
  , "count-selected", "count", "count-non-empty", "sum", "max", "min", "round", "int", "floor", "ceiling", "pow", "log"
  , "log10", "abs", "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sqrt", "exp", "exp10", "pi", "today", "now"
  , "date", "date-time", "decimal-date", "decimal-date-time", "decimal-time", "format-date", "format-date-time", "if"
  , "coalesce", "once", "position", "instance", "current", "random", "uuid", "regex", "checklist", "weighted-checklist"
  , "boolean-from-string", "digest", "base64-decode", "area", "distance", "indexed-repeat", "last"
  ]

-- | Prepare an expression for XPath: booleans are function calls, and raw
-- XLSForm logic has its @${q}@ references resolved to paths.
toXPath :: Env -> Expr -> Expr
toXPath env = go
  where
    go e = case e of
      EBin o (ERef q) (ELit (LBool b)) | Just trig <- M.lookup q (eBools env) -> EBin o (ERef q) (ELit (LStr (boolValue trig b)))
      ELit (LBool b) -> ECall (if b then "true" else "false") []
      ERaw "xlsform" src -> ERaw "xpath" (resolve src)
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      _ -> e
    boolValue trig b
      | trig = if b then "OK" else ""
      | otherwise = if b then "yes" else "no"
    resolve src = case T.breakOn "${" src of
      (pre, "") -> pre
      (pre, rest) -> case T.breakOn "}" (T.drop 2 rest) of
        (q, after) | not (T.null after) -> pre <> " " <> ePath env q <> " " <> resolve (T.drop 1 after)
        _ -> src

strAttr :: Text -> Attrs -> [(Text, Text)]
strAttr prefix a = [(k, v) | (k0, String v) <- M.toList a, Just k <- [T.stripPrefix prefix k0]]

-- | Appearance words: those the type implies, then preserved ones.
appearance :: [Text] -> Attrs -> [(Text, Text)]
appearance derived a =
  let extra = concat [T.words v | k <- ["xform:appearance", "xlsform:appearance"], Just (String v) <- [M.lookup k a]]
      ws = nub (derived ++ extra)
   in [("appearance", T.unwords ws) | not (null ws)]

-- | Known attributes, overridden by preserved ones of the same name.
mergeAttrs :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
mergeAttrs base extra = M.toList (M.union (M.fromList extra) (M.fromList base))

writeItems :: Env -> Text -> [Item] -> W ([X], [X], [X])
writeItems env prefix items = do
  parts <- mapM (writeItem env prefix) items
  pure (concat [a | (a, _, _) <- parts], concat [b | (_, b, _) <- parts], concat [c | (_, _, c) <- parts])

writeItem :: Env -> Text -> Item -> W ([X], [X], [X])
writeItem env prefix = \case
  ItemRaw (Format f) _ -> ([], [], []) <$ lift (warn "xform" ("raw " <> f <> " content dropped"))
  ItemNote i t rel -> do
    n <- nodeName i
    let path = prefix <> "/" <> n
    r <- logic env i rel
    l <- labelX env "label" (path <> ":label") t
    pure
      ( [E n [] []]
      , [E "bind" ([("nodeset", path), ("readonly", "true()"), ("type", "string")] ++ [("relevant", x) | Just x <- [r]]) []]
      , [E "input" [("ref", path)] [l]]
      )
  ItemGroup g -> do
    n <- nodeName (groupId g)
    let path = prefix <> "/" <> n
    r <- logic env (groupId g) (groupRelevant g)
    (inst, binds, body) <- writeItems env path (groupItems g)
    l <- traverse (labelX env "label" (path <> ":label")) (groupTitle g)
    let bindExtra = strAttr "xform:bind:" (groupAttrs g)
        bind = [E "bind" (mergeAttrs ([("nodeset", path)] ++ [("relevant", x) | Just x <- [r]]) bindExtra) [] | isJust r || not (null bindExtra)]
        bodyAttrs = mergeAttrs ([("ref", path)] ++ appearance ["field-list" | groupKind g == Page] (groupAttrs g)) (strAttr "xform:body:" (groupAttrs g))
        ctl = case groupKind g of
          Repeat ->
            E "group" bodyAttrs (maybe [] pure l ++ [E "repeat" (mergeAttrs [("nodeset", path)] (strAttr "xform:repeat:" (groupAttrs g))) body])
          _ -> E "group" bodyAttrs (maybe [] pure l ++ body)
        node = E n [("jr:template", "") | groupKind g == Repeat] inst
    pure ([node], bind ++ binds, [ctl])
  ItemQuestion q -> case qType q of
    QMatrix sel rows cols -> do
      lift (warn (qId q) "matrix written as a field-list group of one select per row")
      when (any (\c -> xmlName (qId q <> "_" <> choiceValue c) /= qId q <> "_" <> choiceValue c) rows || length (nub (map choiceValue rows)) /= length rows) $
        lift (warn (qId q) "matrix row codes changed to make valid, distinct node names")
      let inline = M.lookup "xform:items" (qAttrs q) == Just "inline"
      unless inline . void $ listName env (case M.lookup "xform:list" (qAttrs q) of Just (String l) | not (T.null l) -> l; _ -> xmlName (qId q)) =<< trimCodes (qId q) cols
      -- Row nodes are named <matrix>_<row code>, made valid and distinct.
      let rowNames = snd (foldl pick (S.empty, []) rows)
          pick (used, acc) c =
            let base = xmlName (qId q <> "_" <> choiceValue c)
                n = fromMaybe base (listToMaybe [x | x <- base : [base <> "_" <> T.pack (show k) | k <- [2 :: Int ..]], not (S.member x used)])
             in (S.insert n used, acc ++ [n])
          rowQ (n, c) =
            (question n "" (QChoice sel Radio cols))
              { qLabel = choiceLabel c
              , qRequired = qRequired q
              , qAttrs = M.fromList ([("xform:appearance", "list-nolabel")] ++ [("xform:items", "inline") | inline])
              }
      writeItem env prefix . ItemGroup $
        Group (qId q) Page (Just (qLabel q)) (qRelevant q) (map (ItemQuestion . rowQ) (zip rowNames rows)) M.empty
    _ -> writeQuestion env prefix q

writeQuestion :: Env -> Text -> Question -> W ([X], [X], [X])
writeQuestion env prefix q = do
  n <- nodeName (qId q)
  let path = prefix <> "/" <> n
      note = lift . warn (qId q)
  rel <- logic env (qId q) (qRelevant q)
  con <- logic env (qId q) (qValidation q >>= valExpr)
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
      else logic env (qId q) (conj (bounds ++ [e | isJust con, Just e <- [qValidation q >>= valExpr]]))
  -- A reader trims a default, so one with spaces around it would not
  -- come back the same.
  dflt <- case qDefault q of
    Just d | T.strip d /= d -> (if T.null (T.strip d) then Nothing else Just (T.strip d)) <$ note (if T.null (T.strip d) then "blank default value dropped" else "spaces around the default value trimmed")
    d -> pure d
  msg <- case (rangeCon, qValidation q >>= valMessage) of
    (Just _, Just m)
      | eMulti env -> do
          let i = path <> ":jr:constraintMsg"
          modify' (\w -> w {wsItext = (i, m) : wsItext w})
          pure [("jr:constraintMsg", "jr:itext('" <> i <> "')")]
      | otherwise -> pure [("jr:constraintMsg", i18nDefault m)]
    (Nothing, Just _) -> [] <$ note "validation message without a constraint dropped"
    _ -> pure []
  let bodiless = case qType q of
        QHidden -> True
        QCalculate _ -> True
        _ -> False
  when (bodiless && isJust (qHint q)) $ note "hint of a hidden field dropped"
  label <- if bodiless then pure (E "label" [] []) else labelX env "label" (path <> ":label") (qLabel q)
  hint <- if bodiless then pure Nothing else traverse (labelX env "hint" (path <> ":hint")) (qHint q)
  let lh = label : maybe [] pure hint
      control tag derived extra cs = E tag (mergeAttrs ([("ref", path)] ++ appearance derived (qAttrs q) ++ extra) (strAttr "xform:body:" (qAttrs q))) (lh ++ cs)
  calc <- case qType q of
    QCalculate e -> logic env (qId q) (Just e)
    _ -> pure Nothing
  body <- case qType q of
    QText k -> do
      when (k `elem` [Email, Url]) $ note "email/url input written as plain text"
      pure [control "input" (case k of LongText -> ["multiline"]; Phone -> ["numbers"]; _ -> []) [] []]
    QNumber _ -> pure [control "input" [] [] []]
    QDate _ -> pure [control "input" [] [] []]
    QGeo -> pure [control "input" [] [] []]
    QChoice sel style cs -> do
      when (any choiceExclusive cs) $ note "exclusive choice written as a plain choice"
      when (any choiceOther cs) $ note "other choice written without a specify box"
      is <- itemset path (listHint (xmlName (qId q))) cs
      let derived = case style of
            Dropdown | not (hasWord "autocomplete") -> ["minimal"]
            Buttons | not (hasWord "likert") -> ["quick"]
            _ -> []
      pure [control (if sel == Single then "select1" else "select") derived [] is]
    QRanking cs -> do
      is <- itemset path (listHint (xmlName (qId q))) cs
      pure [control "odk:rank" [] [] is]
    QScale lo hi l h -> do
      when (isJust l || isJust h) $ note "scale end labels dropped"
      pure [control "range" [] [("start", T.pack (show lo)), ("end", T.pack (show hi)), ("step", "1")] []]
    QBoolean
      | M.lookup "xform:control" (qAttrs q) == Just "trigger" -> pure [control "trigger" [] [] []]
      | otherwise -> do
          note "boolean written as a yes/no select"
          is <- itemset path (listHint "yes_no") [choice "yes" "Yes", choice "no" "No"]
          pure [control "select1" [] [] is]
    QFile -> pure [control "upload" [] [("mediatype", "application/*")] []]
    QHidden -> pure []
    QCalculate _ -> pure []
    QOther t -> do
      unless (t `elem` knownBindTypes) $ note ("type " <> t <> " written as a text input")
      pure [control "input" [] [] []]
    QMatrix {} -> pure []
  when (null body && not (T.null (i18nDefault (qLabel q)))) $ note "label of a hidden field dropped"
  -- A required expression read from XLSForm (@${age} >= 18@) is the same
  -- rule in XPath.
  xlsRequired <- case M.lookup "xlsform:required" (qAttrs q) of
    Just (String src) | not (qRequired q), not (M.member "xform:bind:required" (qAttrs q)) -> logic env (qId q) (Just (parseOrRaw xlsform src))
    _ -> pure Nothing
  let bindBase =
        [("nodeset", path), ("type", bindTypeFor (qType q))]
          ++ [("required", "true()") | qRequired q]
          ++ [("required", x) | Just x <- [xlsRequired]]
          ++ [("relevant", x) | Just x <- [rel]]
          ++ [("constraint", x) | Just x <- [rangeCon]]
          ++ [("calculate", x) | Just x <- [calc]]
          ++ msg
      -- A kept required expression is written only while the question
      -- is not simply required.
      keptBind = [(k, v) | (k, v) <- strAttr "xform:bind:" (qAttrs q), not (k == "required" && qRequired q)]
      bind = E "bind" (mergeAttrs bindBase keptBind) []
  pure ([E n [] [T d | Just d <- [dflt]]], [bind], body)
  where
    hasWord w = w `elem` concat [T.words v | k <- ["xform:appearance", "xlsform:appearance"], Just (String v) <- [M.lookup k (qAttrs q)]]
    listHint d = case M.lookup "xform:list" (qAttrs q) of
      Just (String l) | not (T.null l) -> l
      _ -> d
    -- Inline items where the source had them, else a secondary instance
    -- as pyxform writes.
    itemset path hint cs0 = do
      cs <- trimCodes (qId q) cs0
      if M.lookup "xform:items" (qAttrs q) == Just "inline"
        then forM cs $ \c -> do
          l <- labelX env "label" (path <> "/" <> choiceValue c <> ":label") (choiceLabel c)
          pure (E "item" [] [l, E "value" [] [T (choiceValue c)]])
        else do
          n <- listName env hint cs
          let lbl = if eMulti env then E "label" [("ref", "jr:itext(itextId)")] [] else E "label" [("ref", "label")] []
          pure [E "itemset" [("nodeset", "instance('" <> n <> "')/root/item")] [E "value" [("ref", "name")] [], lbl]]

-- | An id as an XML element name: characters an NCName cannot hold become
-- @_@, and a name that cannot start one gets a leading @_@.
xmlName :: Text -> Text
xmlName t = case T.uncons clean of
  Just (c, _) | isAlpha c || c == '_' -> clean
  _ -> "_" <> clean
  where
    clean = T.map (\c -> if isAlphaNum c || c `elem` ("._-" :: String) then c else '_') t

nodeName :: Text -> W Text
nodeName = pure . xmlName

-- | Give every group, question and note a distinct valid XML name (the
-- instance tree is keyed by them, and @meta@ is taken), and follow the
-- renames in logic. Names that are already distinct and valid are kept,
-- so a second write renames nothing.
uniqueNames :: [Item] -> Conv [Item]
uniqueNames items = do
  (renamed, (_, firsts)) <- runStateT (mapM go items) (S.singleton "meta", M.empty)
  let sub q = M.findWithDefault q q firsts
      ids = [itemId i | i <- allItems items, case i of ItemRaw _ _ -> False; _ -> True]
      shared = M.keysSet (M.filter (> (1 :: Int)) (M.fromListWith (+) [(i, 1) | i <- ids]))
      ambiguous = S.toList (S.fromList [(at, r) | (at, e) <- logicOf items, r <- refs e, S.member r shared])
  -- Logic names a question by id alone, so it cannot say which of two
  -- items with one id it means.
  mapM_ (\(at, r) -> warn at ("logic refers to " <> r <> ", an id several items share; it is pointed at the first")) ambiguous
  pure (map (fixRefs sub) renamed)
  where
    logicOf = concatMap $ \case
      ItemGroup g -> [(groupId g, e) | Just e <- [groupRelevant g]] ++ logicOf (groupItems g)
      ItemQuestion q ->
        [(qId q, e) | Just e <- [qRelevant q]]
          ++ [(qId q, e) | Just e <- [qValidation q >>= valExpr]]
          ++ [(qId q, e) | QCalculate e <- [qType q]]
      ItemNote i _ r -> [(i, e) | Just e <- [r]]
      ItemRaw _ _ -> []
    name i = do
      (used, firsts) <- get
      let base = xmlName i
          n = fromMaybe base (listToMaybe [c | c <- base : [base <> "_" <> T.pack (show k) | k <- [2 :: Int ..]], not (S.member c used)])
      put (S.insert n used, M.insertWith (\_ old -> old) i n firsts)
      when (n /= i) $ lift (warn i ("renamed to " <> n <> " to be a distinct XML name"))
      pure n
    go = \case
      ItemGroup g -> do
        n <- name (groupId g)
        kids <- mapM go (groupItems g)
        pure (ItemGroup g {groupId = n, groupItems = kids})
      ItemQuestion q -> (\n -> ItemQuestion q {qId = n}) <$> name (qId q)
      ItemNote i t r -> (\n -> ItemNote n t r) <$> name i
      i -> pure i
    fixRefs sub = \case
      ItemGroup g -> ItemGroup g {groupRelevant = refsIn sub <$> groupRelevant g, groupItems = map (fixRefs sub) (groupItems g)}
      ItemQuestion q ->
        ItemQuestion
          q
            { qRelevant = refsIn sub <$> qRelevant q
            , qValidation = (\v -> v {valExpr = refsIn sub <$> valExpr v}) <$> qValidation q
            , qType = case qType q of
                QCalculate e -> QCalculate (refsIn sub e)
                t -> t
            }
      ItemNote i t r -> ItemNote i t (refsIn sub <$> r)
      i -> i

refsIn :: (Text -> Text) -> Expr -> Expr
refsIn sub = go
  where
    go = \case
      ERef q | q /= "." -> ERef (sub q)
      ESelected q v | q /= "." -> ESelected (sub q) v
      EAnswered q -> EAnswered (sub q)
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      e -> e

-- | Choice codes are element text a reader trims, so surrounding spaces
-- would not survive; trim them here and say so.
trimCodes :: Text -> [Choice] -> W [Choice]
trimCodes at cs = do
  let trimmed = [c {choiceValue = T.strip (choiceValue c)} | c <- cs]
  when (trimmed /= cs) $ lift (warn at "spaces around choice codes trimmed")
  pure trimmed

-- | A secondary instance for a choice list; identical lists share one.
listName :: Env -> Text -> [Choice] -> W Text
listName env hint cs = do
  acc <- gets wsLists
  case [n | (n, cs') <- acc, cs' == cs] of
    (n : _) -> pure n
    [] -> do
      let taken = map fst acc
          n = fromMaybe hint (listToMaybe (filter (`notElem` taken) (hint : [hint <> "_" <> T.pack (show i) | i <- [2 :: Int ..]])))
      modify' (\w -> w {wsLists = acc ++ [(n, cs)]})
      when (eMulti env) $
        modify' (\w -> w {wsItext = reverse [(n <> "-" <> T.pack (show i), choiceLabel c) | (i, c) <- zip [0 :: Int ..] cs] ++ wsItext w})
      pure n
