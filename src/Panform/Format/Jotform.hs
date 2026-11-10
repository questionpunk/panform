-- | Jotform form JSON as the REST API returns it: @questions@ (or the
-- @content@ of @GET /form/{id}/questions@) maps a qid to a stringly-typed
-- @control_*@ field, options are pipe-separated labels, and
-- @properties.conditions@ holds show/hide rules keyed by qid. Pages are
-- delimited by @control_pagebreak@; a @control_head@ opening a page is its
-- title.
module Panform.Format.Jotform (readJotform, writeJotform) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Design (readDesign, writeDesign)
import Panform.Expr (pushNot)
import Panform.Json
import Panform.Jump (conj, disj, neg)

------------------------------------------------------------------------------
-- Reading

readJotform :: BL.ByteString -> Conv Form
readJotform bs = do
  v <- eitherDecode bs `orFail` "Jotform JSON"
  let props = fromMaybe Null (key "properties" v)
      raw = case key "questions" v `orElse` key "content" v of
        Just q -> keyArr' q
        Nothing -> []
      qs = sortOn (\q -> (numOr 0 (keyText "order" q), numOr 0 (keyText "qid" q))) [q | q@(Object _) <- raw]
      title = nonEmpty (keyText "title" props)
      names = M.fromList [(qidOf q, nameOf q) | q <- qs]
      choiceMaps = M.fromList [(nameOf q, choicesOf q) | q <- qs]
      kinds = M.fromList [(nameOf q, keyTextOr "" "type" q) | q <- qs]
  rels <- readConditions names kinds choiceMaps (keyArr' (fromMaybe Null (key "conditions" props)))
  pages <- buildPages qs
  items <- forM pages $ \(pid, ptitle, pqs) -> do
    kids <- catMaybes <$> mapM readField pqs
    pure . ItemGroup $ Group pid Page ptitle Nothing (map (withRel rels) kids) M.empty
  let meta =
        emptyMeta
          { metaId = keyText "id" props
          , metaTitle = plain <$> title
          , metaAttrs = readDesign "jotform" props
          }
  pure (Form meta items)
  where
    orElse a b = maybe b Just a
    withRel rels = \case
      ItemQuestion q -> ItemQuestion q {qRelevant = M.lookup (qId q) rels}
      ItemNote i t _ -> ItemNote i t (M.lookup i rels)
      i -> i

-- | An array, or the values of a qid-keyed object.
keyArr' :: Value -> [Value]
keyArr' = \case
  Array a -> foldr (:) [] a
  Object o -> KM.elems o
  String s | Right x <- eitherDecode (BL.fromStrict (TE.encodeUtf8 s)) -> keyArr' x
  _ -> []

numOr :: Int -> Maybe Text -> Int
numOr d = \case
  Just t | [(n, "")] <- reads (T.unpack t) -> n
  _ -> d

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null (T.strip t)) -> Just t
  _ -> Nothing

qidOf :: Value -> Text
qidOf = keyTextOr "" "qid"

nameOf :: Value -> Text
nameOf q = fromMaybe ("q" <> qidOf q) (nonEmpty (keyText "name" q))

pipes :: Maybe Text -> [Text]
pipes = maybe [] (filter (not . T.null) . T.splitOn "|")

-- | Choices of a choice field: labels, coded by @calcValues@ when it has
-- one value per option.
choicesOf :: Value -> [Choice]
choicesOf q =
  [Choice v (plain l) False False | (v, l) <- zip values labels]
    ++ [Choice other (plain other) True False | keyText "allowOther" q == Just "Yes"]
  where
    labels = pipes (keyText "options" q)
    -- codes may be empty, so split without dropping empty entries
    calc = maybe [] (T.splitOn "|") (keyText "calcValues" q)
    values = if length calc == length labels then calc else labels
    other = keyTextOr "Other" "otherText" q

-- | Split the field list at page breaks. A page's id is the name of the
-- break before it; a header opening a page is its title.
buildPages :: [Value] -> Conv [(Text, Maybe I18n, [Value])]
buildPages = go (1 :: Int) "page1" []
  where
    go n pid acc = \case
      [] -> pure [page pid (reverse acc)]
      (q : rest)
        | keyText "type" q == Just "control_pagebreak" -> do
            let next = fromMaybe ("page" <> T.pack (show (n + 1))) (nonEmpty (keyText "name" q))
            (page pid (reverse acc) :) <$> go (n + 1) next [] rest
        | keyText "type" q == Just "control_button" -> go n pid acc rest
        | otherwise -> go n pid (q : acc) rest
    page pid qs = case qs of
      (h : rest)
        | keyText "type" h == Just "control_head" ->
            (pid, Just (plain (keyTextOr "" "text" h)), rest)
      _ -> (pid, Nothing, qs)

-- | Keys the reader maps into the IR; the rest are kept as
-- @jotform:<key>@ attributes.
consumed :: [Text]
consumed =
  [ "qid", "type", "name", "text", "order", "required", "options", "calcValues", "allowOther", "otherText", "subLabel"
  , "defaultValue", "hidden", "minValue", "maxValue", "mrows", "mcolumns", "inputType", "scaleAmount", "scaleFrom"
  , "fromText", "toText", "stars", "showTime"
  ]

readField :: Value -> Conv (Maybe Item)
readField q = case ty of
  "control_text" -> pure (Just (ItemNote name (plain text) Nothing))
  "control_head" -> do
    when (isJust (nonEmpty (keyText "subHeader" q))) $ warn name "header subtitle dropped"
    pure (Just (ItemNote name (plain text) Nothing))
  _ -> do
    qt <- readType
    pure . Just . ItemQuestion $
      Question
        { qId = name
        , qLabel = plain text
        , qHint = plain <$> nonEmpty (keyText "subLabel" q)
        , qType = qt
        , qRequired = keyText "required" q == Just "Yes"
        , qRelevant = Nothing
        , qValidation = validation
        , qDefault = nonEmpty (keyText "defaultValue" q)
        , qAttrs =
            M.fromList $
              [("jotform:" <> k, x) | k <- attrKeys, Just x <- [key k q]]
                ++ [("jotform:type", String ty) | ty == "control_rating"]
        }
  where
    ty = keyTextOr "" "type" q
    name = nameOf q
    text = keyTextOr "" "text" q
    attrKeys = case q of
      Object o -> [K.toText k | k <- KM.keys o, K.toText k `notElem` consumed]
      _ -> []
    validation =
      let lo = keyNum "minValue" q
          hi = keyNum "maxValue" q
       in if ty == "control_number" && (isJust lo || isJust hi) then Just (Validation lo hi Nothing Nothing) else Nothing
    readType = case ty of
      "control_textbox"
        | keyText "hidden" q == Just "Yes" -> pure QHidden
        | otherwise -> pure $ case keyText "validation" q of
            Just "Email" -> QText Email
            Just "Numeric" -> QNumber Decimal
            Just "Url" -> QText Url
            _ -> QText ShortText
      "control_textarea" -> pure (QText LongText)
      "control_email" -> pure (QText Email)
      "control_phone" -> pure (QText Phone)
      "control_number" -> pure (QNumber Decimal)
      "control_radio" -> pure (QChoice Single Radio (choicesOf q))
      "control_checkbox" -> pure (QChoice Multiple Radio (choicesOf q))
      "control_dropdown" -> pure (QChoice (if keyText "multipleSelections" q == Just "Yes" then Multiple else Single) Dropdown (choicesOf q))
      "control_scale" ->
        pure (QScale (numOr 1 (keyText "scaleFrom" q)) (numOr 5 (keyText "scaleAmount" q)) (plain <$> nonEmpty (keyText "fromText" q)) (plain <$> nonEmpty (keyText "toText" q)))
      "control_rating" -> pure (QScale 1 (numOr 5 (keyText "stars" q)) Nothing Nothing)
      "control_matrix" -> do
        let sel = case keyText "inputType" q of
              Just "Check Box" -> pure Multiple
              Just "Radio Button" -> pure Single
              Nothing -> pure Single
              Just other -> Single <$ warn name ("matrix of " <> other <> " cells written as a radio matrix")
        s <- sel
        pure (QMatrix s [choice l l | l <- pipes (keyText "mrows" q)] [choice l l | l <- pipes (keyText "mcolumns" q)])
      "control_datetime" -> pure (QDate (if keyText "showTime" q == Just "Yes" then DateTime else Date))
      "control_time" -> pure (QDate Time)
      "control_fileupload" -> pure QFile
      t -> pure (QOther t)

-- | Show/hide conditions per target field, as show-if expressions keyed by
-- field name.
readConditions :: M.Map Text Text -> M.Map Text Text -> M.Map Text [Choice] -> [Value] -> Conv (M.Map Text Expr)
readConditions names kinds choiceMaps conds = do
  parts <- fmap concat . forM conds $ \c -> case keyTextOr "field" "type" c of
    "field" -> case readTerms c of
      Left why -> [] <$ warn "jotform" ("condition dropped, " <> why)
      Right e -> fmap catMaybes . forM (keyArr' (fromMaybe Null (key "action" c))) $ \a -> do
        let targets = mapMaybe (`M.lookup` names) (maybe [] pure (keyText "field" a) ++ mapMaybe asText (keyArr' (fromMaybe Null (key "fields" a))))
        case keyText "visibility" a of
          Just vis | vis `elem` ["Show", "ShowMultiple"] -> pure (Just [(t, (True, e)) | t <- targets])
          Just vis | vis `elem` ["Hide", "HideMultiple"] -> pure (Just [(t, (False, e)) | t <- targets])
          other -> Nothing <$ warn "jotform" ("condition action " <> fromMaybe "?" other <> " dropped")
    t -> [] <$ warn "jotform" (t <> " condition dropped")
  let byTarget = M.fromListWith (flip (++)) [(t, [x]) | ps <- parts, (t, x) <- ps]
  pure . M.mapMaybe id $ flip M.map byTarget $ \xs ->
    conj (maybe [] pure (disj [e | (True, e) <- xs]) ++ [neg e | (False, e) <- xs])
  where
    readTerms c = do
      terms <- mapM readTerm (keyArr' (fromMaybe Null (key "terms" c)))
      maybe (Left "no terms") Right $ if keyText "link" c == Just "Any" then disj terms else conj terms
    readTerm t = do
      q <- maybe (Left ("unknown field " <> keyTextOr "" "field" t)) Right (keyText "field" t >>= (`M.lookup` names))
      let op = keyTextOr "" "operator" t
          raw = keyTextOr "" "value" t
          kind = M.findWithDefault "" q kinds
          multi = kind == "control_checkbox"
          value = case [choiceValue c | c <- M.findWithDefault [] q choiceMaps, i18nDefault (choiceLabel c) == raw] of
            (v : _) -> v
            [] -> raw
          lit
            | kind `elem` numericKinds, [(n, "")] <- reads (T.unpack raw) = ELit (LNum n)
            | otherwise = ELit (LStr value)
      pure $ case op of
        "equals" | multi -> ESelected q value
        "notEquals" | multi -> ENot (ESelected q value)
        "contains" | multi -> ESelected q value
        "notContains" | multi -> ENot (ESelected q value)
        "equals" -> EBin Eq (ERef q) lit
        "notEquals" -> EBin Neq (ERef q) lit
        "lessThan" -> EBin Lt (ERef q) lit
        "greaterThan" -> EBin Gt (ERef q) lit
        "isFilled" -> EAnswered q
        "isEmpty" -> ENot (EAnswered q)
        _ -> ECall op (ERef q : [ELit (LStr raw) | not (T.null raw)])

numericKinds :: [Text]
numericKinds = ["control_number", "control_scale", "control_rating", "control_spinner"]

------------------------------------------------------------------------------
-- Writing

-- | A field to write, with its qid and the condition it is shown under.
data Field = Field
  { fQid :: Int
  , fItem :: Either (Text, Text) Item
  -- ^ A structural control (type, name) or an item.
  , fTitle :: Maybe Text
  , fRel :: Maybe Expr
  }

writeJotform :: Form -> Conv BL.ByteString
writeJotform s@(Form meta items) = do
  when (any (not . M.null . i18nTranslations) (catMaybes [metaTitle meta, metaDescription meta]) || hasTranslations s) $
    warn "jotform" "translations dropped, Jotform keeps them outside the form"
  when (isJust (metaDescription meta)) $ warn "jotform" "survey description dropped"
  laid <- layout items
  let (kept, dropped) = partitionOn (writable . fItem . ($ 0)) laid
  mapM_ (writeField . ($ 0)) dropped
  let numbered = zipWith (\n f -> f n) [1 ..] (kept ++ [\n -> Field n (Left ("control_button", "submit")) (Just "Submit") Nothing])
      fields = uniqueNames numbered
      -- Logic names fields by their old names; the first of a repeated
      -- name keeps it.
      qids = M.fromListWith (\_ first -> first) [(itemId i, T.pack (show (fQid f))) | f <- numbered, Right i <- [fItem f], not (T.null (itemId i))]
      kinds = M.fromListWith (\_ first -> first) [(qId q, qType q) | ItemQuestion q <- allItems items]
  sequence_
    [ warn (if T.null (itemId i) then "jotform" else itemId i) ("field name written as " <> itemId i')
    | (f, f') <- zip numbered fields
    , Right i <- [fItem f]
    , Right i' <- [fItem f']
    , itemId i /= itemId i'
    ]
  qs <- fmap catMaybes . forM fields $ \f -> fmap (\fs -> (T.pack (show (fQid f)), fs)) <$> writeField f
  conds <- fmap catMaybes . forM fields $ \f -> case (fItem f, fRel f) of
    (Right i, Just e) -> case condition qids kinds e of
      Left why -> Nothing <$ warn (itemId i) ("logic dropped, jotform cannot express " <> why)
      Right (link, terms) ->
        pure . Just $
          object
            [ "type" .= ("field" :: Text)
            , "link" .= (link :: Text)
            , "terms" .= terms
            , "action" .= [object ["visibility" .= ("Show" :: Text), "field" .= T.pack (show (fQid f))]]
            ]
    _ -> pure Nothing
  pure . encodePretty $
    object
      [ "properties" .= obj ([("id" .=?) . String =<< metaId meta, ("title" .=?) . String . i18nDefault =<< metaTitle meta, if null conds then Nothing else "conditions" .=? toJSON conds] ++ map Just (writeDesign "jotform" meta))
      , "questions" .= object [(K.fromText k, object [(K.fromText a, b) | (a, b) <- fs]) | (k, fs) <- qs]
      ]
  where
    hasTranslations (Form _ is) = any itemTr (allItems is)
    itemTr = \case
      ItemQuestion q -> tr (qLabel q) || any tr (qHint q) || any tr (labels (qType q))
      ItemNote _ t _ -> tr t
      ItemGroup g -> any tr (groupTitle g)
      ItemRaw _ _ -> False
    tr = not . M.null . i18nTranslations
    labels = \case
      QChoice _ _ cs -> map choiceLabel cs
      QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
      QRanking cs -> map choiceLabel cs
      QScale _ _ l h -> catMaybes [l, h]
      _ -> []

-- | Jotform names must be present and unique: an empty or repeated name
-- becomes @q<qid>@, as the reader names a nameless field, or @q<qid>_2@
-- and so on when another field has that name.
uniqueNames :: [Field] -> [Field]
uniqueNames fs = go S.empty fs
  where
    controls = S.fromList [n | f <- fs, Left (_, n) <- [fItem f]]
    claimed = S.fromList [itemId i | f <- fs, Right i <- [fItem f], isNamed i]
    go _ [] = []
    go seen (f : rest) = case fItem f of
      Right i | isNamed i ->
        let n = itemId i
            base = "q" <> T.pack (show (fQid f))
            free c = all (S.notMember c) [seen, claimed, controls]
            n'
              | not (T.null n) && S.notMember n seen && S.notMember n controls = n
              | otherwise = case filter free (base : [base <> "_" <> T.pack (show k) | k <- [2 :: Int ..]]) of
                  (c : _) -> c
                  [] -> base
         in f {fItem = Right (rename n' i)} : go (S.insert n' seen) rest
      _ -> f : go seen rest
    isNamed = \case
      ItemQuestion _ -> True
      ItemNote {} -> True
      _ -> False
    rename n = \case
      ItemQuestion q -> ItemQuestion q {qId = n}
      ItemNote _ t r -> ItemNote n t r
      i -> i

-- | Pages become break-separated runs of fields, a page title a header
-- opening its page; nested groups are flattened, their conditions pushed
-- onto their members.
layout :: [Item] -> Conv [Int -> Field]
layout items = concat <$> mapM page (zip [1 :: Int ..] (paginate items))
  where
    page (n, (g, kids)) = do
      let gid = maybe ("page" <> T.pack (show n)) groupId g
      when (n == 1 && gid /= "page1") $ warn gid "first page id not carried"
      when (isJust (g >>= groupRelevant)) $ warn gid "page condition copied onto its fields"
      body <- concat <$> mapM (flatten (g >>= groupRelevant)) kids
      let brk = [\q -> Field q (Left ("control_pagebreak", gid)) Nothing Nothing | n > 1]
          hdr = [\q -> Field q (Left ("control_head", (if n == 1 then "page1" else gid) <> "_title")) (Just (i18nDefault t)) Nothing | Just t <- [g >>= groupTitle]]
      pure (brk ++ hdr ++ body)
    flatten rel = \case
      ItemGroup g -> do
        warn (groupId g) "group flattened into its page"
        let rel' = conj (catMaybes [rel, groupRelevant g])
            hdr = [\q -> Field q (Right (ItemNote (groupId g) t Nothing)) Nothing rel' | Just t <- [groupTitle g]]
        (hdr ++) . concat <$> mapM (flatten rel') (groupItems g)
      i -> pure [\q -> Field q (Right i) Nothing (conj (catMaybes [rel, own i]))]
    own = \case
      ItemQuestion q -> qRelevant q
      ItemNote _ _ r -> r
      _ -> Nothing

partitionOn :: (a -> Bool) -> [a] -> ([a], [a])
partitionOn p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])

-- | Whether 'writeField' emits anything; fields that are dropped take no
-- qid, so qids stay dense.
writable :: Either (Text, Text) Item -> Bool
writable = \case
  Left _ -> True
  Right (ItemNote {}) -> True
  Right (ItemRaw (Format "jotform") (Object _)) -> True
  Right (ItemQuestion q) -> case qType q of
    QCalculate _ -> False
    QOther t -> "control_" `T.isPrefixOf` t
    _ -> True
  Right _ -> False

paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

writeField :: Field -> Conv (Maybe [(Text, Value)])
writeField f = case fItem f of
  Left (ty, name) -> pure (Just (base ty name (fromMaybe "" (fTitle f))))
  Right (ItemNote i t _) -> pure (Just (base "control_text" i (i18nDefault t)))
  Right (ItemRaw (Format "jotform") (Object o)) -> pure (Just ([(K.toText k, x) | (k, x) <- KM.toList o] ++ ids))
  Right (ItemRaw (Format fm) _) -> Nothing <$ warn "jotform" ("raw " <> fm <> " content dropped")
  Right (ItemGroup g) -> Nothing <$ warn (groupId g) "group dropped"
  Right (ItemQuestion q) -> do
    when (isJust (qValidation q >>= valExpr)) $ warn (qId q) "validation expression dropped"
    when (isJust (qValidation q >>= valMessage)) $ warn (qId q) "validation message dropped"
    fields <- typeFields q
    pure $ flip fmap fields $ \(ty, fs) ->
      let core =
            base ty (qId q) (i18nDefault (qLabel q))
              ++ [("required", if qRequired q then "Yes" else "No")]
              ++ [("subLabel", String (i18nDefault h)) | Just h <- [qHint q]]
              ++ [("defaultValue", String d) | Just d <- [qDefault q]]
              ++ fs
          attrs = [(k', x) | (k, x) <- M.toList (qAttrs q), Just k' <- [T.stripPrefix "jotform:" k], k' /= "type", k' `notElem` map fst core]
       in core ++ attrs
  where
    ids = [("qid", String qid), ("order", String qid)]
    qid = T.pack (show (fQid f))
    base ty name text = [("type", String ty), ("name", String name), ("text", String text)] ++ ids

-- | Options are pipe-separated labels, so a label can be neither empty
-- nor contain a pipe.
pipeSafe :: Choice -> Choice
pipeSafe c = c {choiceValue = clean (choiceValue c), choiceLabel = (choiceLabel c) {i18nDefault = label}}
  where
    clean = T.replace "|" "/"
    l = clean (i18nDefault (choiceLabel c))
    label
      | not (T.null (T.strip l)) = l
      | not (T.null (T.strip (clean (choiceValue c)))) = clean (choiceValue c)
      | otherwise = "-"

pipeSafeType :: QType -> QType
pipeSafeType = \case
  QChoice s st cs -> QChoice s st (map pipeSafe cs)
  QMatrix s rs cs -> QMatrix s (map pipeSafe rs) (map pipeSafe cs)
  QRanking cs -> QRanking (map pipeSafe cs)
  t -> t

typeFields :: Question -> Conv (Maybe (Text, [(Text, Value)]))
typeFields q0 = do
  let q = q0 {qType = pipeSafeType (qType q0)}
  when (qType q /= qType q0) $ warn (qId q) "empty or pipe-containing option labels rewritten"
  typeFields' q

typeFields' :: Question -> Conv (Maybe (Text, [(Text, Value)]))
typeFields' q = case qType q of
  QText k -> pure . Just $ case k of
    ShortText -> ("control_textbox", [])
    LongText -> ("control_textarea", [])
    Email -> ("control_email", [])
    Url -> ("control_textbox", [("validation", "Url")])
    Phone -> ("control_phone", [])
  QNumber k -> do
    when (k == Integer) $ warn (qId q) "integer constraint dropped"
    let v = qValidation q
    pure (Just ("control_number", [("minValue", String (showNum x)) | Just x <- [v >>= valMin]] ++ [("maxValue", String (showNum x)) | Just x <- [v >>= valMax]]))
  QChoice sel style cs -> do
    let others = [c | c <- cs, choiceOther c]
        regular = [c | c <- cs, not (choiceOther c)]
        coded = any (\c -> choiceValue c /= i18nDefault (choiceLabel c)) regular
        ty = case (sel, style) of
          (Single, Dropdown) -> "control_dropdown"
          (Multiple, Dropdown) -> "control_dropdown"
          (Single, _) -> "control_radio"
          (Multiple, _) -> "control_checkbox"
    when (length others > 1) $ warn (qId q) "only one other choice kept"
    sequence_ [warn (qId q) ("other option code " <> choiceValue o <> " becomes its label") | o <- take 1 others, choiceValue o /= i18nDefault (choiceLabel o)]
    unless (null [c | c <- regular, choiceExclusive c]) $ warn (qId q) "exclusive choice written as a plain choice"
    pure . Just . (ty,) $
      [("options", String (T.intercalate "|" (map (i18nDefault . choiceLabel) regular)))]
        ++ [("calcValues", String (T.intercalate "|" (map choiceValue regular))) | coded]
        ++ concat [[("allowOther", "Yes"), ("otherText", String (i18nDefault (choiceLabel o)))] | o <- take 1 others]
        ++ [("multipleSelections", "Yes") | sel == Multiple, style == Dropdown]
  QMatrix sel rows cols -> do
    when (any (\c -> choiceValue c /= i18nDefault (choiceLabel c)) (rows ++ cols)) $ warn (qId q) "matrix codes replaced by labels"
    pure . Just . ("control_matrix",) $
      [ ("mrows", String (T.intercalate "|" (map (i18nDefault . choiceLabel) rows)))
      , ("mcolumns", String (T.intercalate "|" (map (i18nDefault . choiceLabel) cols)))
      , ("inputType", if sel == Single then "Radio Button" else "Check Box")
      ]
  QScale lo hi l h
    | M.lookup "jotform:type" (qAttrs q) == Just "control_rating", lo == 1 -> pure (Just ("control_rating", [("stars", String (tshow hi))]))
    | lo `elem` [0, 1] ->
        pure . Just . ("control_scale",) $
          [("scaleFrom", String (tshow lo)), ("scaleAmount", String (tshow hi))]
            ++ [("fromText", String (i18nDefault x)) | Just x <- [l]]
            ++ [("toText", String (i18nDefault x)) | Just x <- [h]]
    | otherwise -> do
        warn (qId q) ("scale " <> tshow lo <> "-" <> tshow hi <> " written as a number box")
        pure (Just ("control_number", [("minValue", String (tshow lo)), ("maxValue", String (tshow hi))]))
  QRanking cs -> do
    warn (qId q) "ranking written as a checkbox list"
    pure (Just ("control_checkbox", [("options", String (T.intercalate "|" (map (i18nDefault . choiceLabel) cs)))]))
  QDate k -> pure . Just $ case k of
    Date -> ("control_datetime", [])
    DateTime -> ("control_datetime", [("showTime", "Yes")])
    Time -> ("control_time", [])
  QBoolean -> do
    warn (qId q) "yes/no written as a two-option radio"
    pure (Just ("control_radio", [("options", "Yes|No")]))
  QFile -> pure (Just ("control_fileupload", []))
  QHidden -> pure (Just ("control_textbox", [("hidden", "Yes")]))
  QGeo -> Just ("control_textbox", []) <$ warn (qId q) "geopoint written as a text box"
  QCalculate _ -> Nothing <$ warn (qId q) "calculated value dropped"
  QOther t
    | "control_" `T.isPrefixOf` t -> pure (Just (t, []))
    | otherwise -> Nothing <$ warn (qId q) ("type " <> t <> " dropped")
  where
    tshow = T.pack . show

showNum :: Double -> Text
showNum d
  | d == fromIntegral r = T.pack (show r)
  | otherwise = T.pack (show d)
  where
    r = round d :: Integer

-- | A condition as a link (All/Any) and flat terms. Jotform cannot nest
-- @and@ inside @or@.
condition :: M.Map Text Text -> M.Map Text QType -> Expr -> Either Text (Text, [Value])
condition qids kinds e = case e of
  EBin Or _ _ -> ("Any",) <$> mapM term (flatten Or e)
  EBin And _ _ -> ("All",) <$> mapM term (flatten And e)
  ENot x@(EBin op _ _) | op `elem` [And, Or] -> maybe (Left "negated group") (condition qids kinds) (pushNot x)
  _ -> ("All",) . pure <$> term e
  where
    flatten op x = case x of
      EBin o a b | o == op -> flatten op a ++ flatten op b
      _ -> [x]
    term x = case x of
      EBin op (ERef q) (ELit l) -> cmp op q l
      ESelected q v -> t q "equals" (label q v)
      ENot (ESelected q v) -> t q "notEquals" (label q v)
      EAnswered q -> t q "isFilled" ""
      ENot (EAnswered q) -> t q "isEmpty" ""
      ECall op [ERef q] -> t q op ""
      ECall op [ERef q, ELit (LStr v)] -> t q op v
      ENot y -> maybe (Left "negation") term (pushNot y)
      EBin op _ _ | op `elem` [And, Or] -> Left "nested and/or"
      _ -> Left (T.pack (take 60 (show x)))
    cmp op q l = case (op, l) of
      (Eq, _) -> t q "equals" (litText q l)
      (Neq, _) -> t q "notEquals" (litText q l)
      (Lt, LNum n) -> t q "lessThan" (showNum n)
      (Gt, LNum n) -> t q "greaterThan" (showNum n)
      (Le, LNum n) | integral q n -> t q "lessThan" (showNum (n + 1))
      (Ge, LNum n) | integral q n -> t q "greaterThan" (showNum (n - 1))
      _ -> Left ("comparison " <> T.pack (show op) <> " on " <> q)
    integral q n = n == fromIntegral (round n :: Integer) && case M.lookup q kinds of
      Just (QScale {}) -> True
      Just (QNumber Integer) -> True
      _ -> False
    litText q = \case
      LStr v -> label q v
      LNum n -> label q (showNum n)
      LBool b -> if b then "true" else "false"
    label q v = case M.lookup q kinds of
      Just (QChoice _ _ cs) | (c : _) <- [pipeSafe c | c <- cs, choiceValue c == v] -> i18nDefault (choiceLabel c)
      _ -> v
    t q op v = case M.lookup q qids of
      Nothing -> Left ("reference to " <> q)
      Just f -> Right (object ["field" .= f, "operator" .= (op :: Text), "value" .= (v :: Text)])
