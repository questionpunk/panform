-- | Typeform Create API form JSON (the body of @POST /forms@ and the result
-- of @GET /forms/{id}@). @fields@ are shown one per screen, @group@ fields
-- nest one level, a field's @ref@ is its stable id, @hidden@ lists URL
-- parameters, and logic is a list of jump rules per field. Jumps that skip
-- forward are converted to and from show-if relevance; variable arithmetic
-- and backward jumps are kept only for Typeform. Server state (@id@,
-- @_links@, timestamps) is not part of the definition and is not read.
module Panform.Format.Typeform (readTypeform, writeTypeform) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.State.Strict (evalStateT, get, lift, put)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Traversable (mapAccumM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Bdd (Bdd, bAnd, bAnds, bNot, bOrs, false, fromExpr, runBdd, toExpr, true)
import qualified Panform.Bdd as Bdd
import Panform.Conv
import Panform.Json

attrKey :: Text -> Text
attrKey = ("typeform:" <>)

tshow :: Int -> Text
tshow = T.pack . show

-- | Form-level keys kept verbatim for a Typeform round trip.
passthrough :: [Text]
passthrough = ["type", "theme", "workspace", "welcome_screens", "thankyou_screens", "variables", "cui_settings", "custom_messages"]

------------------------------------------------------------------------------
-- Shared: the flattened field order logic is computed over

-- | A field in display order: ref, index one past its subtree, ancestor
-- group indices, whether it is a group, and its relevance.
data Entry = Entry
  { eRef :: Text
  , eEnd :: Int
  , eParents :: [Int]
  , eGroup :: Bool
  , eRel :: Maybe Expr
  }

-- | A field tree: ref, relevance, children when it is a group.
data Tree = Tree Text (Maybe Expr) (Maybe [Tree])

flatten :: [Tree] -> [Entry]
flatten = snd . go 0 []
  where
    go i _ [] = (i, [])
    go i ps (Tree r rel kids : rest) = case kids of
      Just ks ->
        let (j, inner) = go (i + 1) (i : ps) ks
            (k, more) = go j ps rest
         in (k, Entry r j ps True rel : inner ++ more)
      Nothing ->
        let (k, more) = go (i + 1) ps rest
         in (k, Entry r (i + 1) ps False rel : more)

chain :: BinOp -> Expr -> [Expr]
chain o = \case
  EBin o' a b | o' == o -> chain o a ++ chain o b
  x -> [x]

------------------------------------------------------------------------------
-- Reading

readTypeform :: BL.ByteString -> Conv Form
readTypeform bs = do
  v <- eitherDecode bs `orFail` "Typeform JSON"
  -- A GET lists the ending screens again as fields of type thankyou_screen.
  let (endingFields, fields) = partitionEithers [if keyText "type" f == Just "thankyou_screen" then Left f else Right f | f <- keyArr "fields" v]
  items <- readFields "field" fields
  let hidden = mapMaybe asText (keyArr "hidden" v)
      types = M.fromList [(qId q, qType q) | ItemQuestion q <- allItems items]
      es = flatten (trees items)
      endings = length (keyArr "thankyou_screens" v)
  (rels, rawLogic) <- readLogic types hidden endings es (keyArr "logic" v)
  let settings = key "settings" v
      lang = settings >>= keyText "language"
      settingsRest = case settings of
        Just (Object o) | o' <- KM.delete "language" o, not (KM.null o') -> Just (Object o')
        _ -> Nothing
      meta =
        emptyMeta
          { metaTitle = plain <$> keyText "title" v
          , metaLanguages = maybe [] pure lang
          , metaAttrs =
              M.fromList $
                [(attrKey "settings", s) | Just s <- [settingsRest]]
                  ++ [(attrKey k, x) | k <- passthrough, Just x <- [key k v]]
                  ++ [(attrKey "logic", toJSON rawLogic) | not (null rawLogic)]
                  ++ [(attrKey "ending_fields", toJSON endingFields) | not (null endingFields)]
          }
  pure (Form meta ([ItemQuestion (question h h QHidden) | h <- hidden] ++ map (applyRel rels) items))

trees :: [Item] -> [Tree]
trees = mapMaybe $ \case
  ItemGroup g -> Just (Tree (groupId g) (groupRelevant g) (Just (trees (groupItems g))))
  ItemQuestion q | qType q /= QHidden -> Just (Tree (qId q) (qRelevant q) Nothing)
  ItemNote i _ rel -> Just (Tree i rel Nothing)
  ItemRaw (Format "typeform") v -> (\r -> Tree r Nothing Nothing) <$> keyText "ref" v
  _ -> Nothing

applyRel :: M.Map Text Expr -> Item -> Item
applyRel rels = \case
  ItemGroup g -> ItemGroup g {groupRelevant = M.lookup (groupId g) rels, groupItems = map (applyRel rels) (groupItems g)}
  ItemQuestion q | qType q /= QHidden -> ItemQuestion q {qRelevant = M.lookup (qId q) rels}
  ItemNote i t _ -> ItemNote i t (M.lookup i rels)
  i -> i

readFields :: Text -> [Value] -> Conv [Item]
readFields prefix fs = catMaybes <$> mapM (\(n, f) -> readField (prefix <> tshow n) f) (zip [1 :: Int ..] fs)

-- | The object without the given keys, or 'Nothing' when nothing is left.
leftover :: [Text] -> Maybe Value -> Maybe Value
leftover ks = \case
  Just (Object o) | o' <- foldr (KM.delete . K.fromText) o ks, not (KM.null o') -> Just (Object o')
  _ -> Nothing

readField :: Text -> Value -> Conv (Maybe Item)
readField fallback f = case ty of
  "group" -> do
    kids <- readFields (ref <> "_") (maybe [] (keyArr "fields") props)
    pure . Just . ItemGroup $
      Group
        { groupId = ref
        , groupKind = Page
        , groupTitle = plain <$> keyText "title" f
        , groupRelevant = Nothing
        , groupItems = kids
        , groupAttrs = M.fromList [(attrKey "properties", p) | Just p <- [leftover ["fields"] props]]
        }
  "statement" -> do
    when (isJust (leftover ["description"] props)) $ warn ref "statement button settings dropped"
    when (isJust (keyText "title" f) && isJust (props >>= keyText "description")) $
      warn ref "statement description joined to its text"
    let body = T.intercalate "\n\n" (catMaybes [keyText "title" f, props >>= keyText "description"])
    pure (Just (ItemNote ref (plain body) Nothing))
  _ -> do
    (qt, used, usedVal, extra) <- readType ref ty f
    let known = qt /= QOther ty
        attrs =
          [(attrKey "properties", p) | known, Just p <- [leftover ("description" : used) props]]
            ++ [(attrKey "validations", p) | known, Just p <- [leftover ("required" : usedVal) vals]]
            ++ [(attrKey k, x) | known, k <- ["attachment", "layout"], Just x <- [key k f]]
            ++ [(attrKey "field", f) | not known]
            ++ extra
    pure . Just . ItemQuestion $
      Question
        { qId = ref
        , qLabel = plain (keyTextOr "" "title" f)
        , qHint = plain <$> (props >>= keyText "description")
        , qType = qt
        , qRequired = (vals >>= keyBool "required") == Just True
        , qRelevant = Nothing
        , qValidation = readValidation ty vals
        , qDefault = Nothing
        , qAttrs = M.fromList attrs
        }
  where
    ty = keyTextOr "" "type" f
    ref = fromMaybe fallback (keyText "ref" f `orElse` keyText "id" f)
    props = key "properties" f
    vals = key "validations" f
    orElse a b = maybe b Just a

-- | The IR type, the property and validation keys it consumed, and extra
-- attributes.
readType :: Text -> Text -> Value -> Conv (QType, [Text], [Text], [(Text, Value)])
readType ref ty f = case ty of
  "short_text" -> pure (QText ShortText, [], ["max_length"], [])
  "long_text" -> pure (QText LongText, [], ["max_length"], [])
  "email" -> pure (QText Email, [], [], [])
  "phone_number" -> pure (QText Phone, [], [], [])
  "website" -> pure (QText Url, [], [], [])
  "number" -> pure (QNumber Integer, [], ["min_value", "max_value"], [])
  "date" -> pure (QDate Date, [], [], [])
  "file_upload" -> pure (QFile, [], [], [])
  "yes_no" -> pure (QBoolean, [], [], [])
  "legal" -> pure (QBoolean, [], [], [typeAttr])
  "multiple_choice" -> choiceQ Radio
  "picture_choice" -> do
    warn ref "choice pictures dropped"
    (t, u, v, _) <- choiceQ Buttons
    pure (t, u, v, [typeAttr])
  "dropdown" -> choiceQ Dropdown
  "ranking" -> do
    cs <- choices ref (propArr "choices")
    pure (QRanking cs, choiceKeys, [], [])
  "rating" -> pure (QScale 1 (maybe 5 round (propNum "steps")) Nothing Nothing, ["steps"], [], [typeAttr])
  "opinion_scale" ->
    let lo = if (props >>= keyBool "start_at_one") == Just True then 1 else 0
     in pure (QScale lo (lo + maybe 11 round (propNum "steps") - 1) (label "left") (label "right"), scaleKeys, [], center)
  "nps" -> pure (QScale 0 10 (label "left") (label "right"), scaleKeys, [], typeAttr : center)
  "matrix" -> do
    let rows = propArr "fields"
        first = case rows of
          (r : _) -> r
          [] -> object []
        sel = if (key "properties" first >>= keyBool "allow_multiple_selection") == Just True then Multiple else Single
    cols <- choices ref (maybe [] (keyArr "choices") (key "properties" first))
    let rowChoices = [Choice (keyTextOr t "ref" r) (plain t) False False | r <- rows, let t = keyTextOr "" "title" r]
    pure (QMatrix sel rowChoices cols, ["fields"], [], [])
  _ -> do
    warn ref ("unsupported type " <> ty <> " kept only for typeform")
    pure (QOther ty, [], [], [])
  where
    props = key "properties" f
    propArr k = maybe [] (keyArr k) props
    propNum k = props >>= keyNum k
    label side = plain <$> (props >>= key "labels" >>= keyText side)
    center = [(attrKey "center_label", String c) | Just c <- [props >>= key "labels" >>= keyText "center"]]
    typeAttr = (attrKey "type", String ty)
    scaleKeys = ["steps", "start_at_one", "labels"]
    choiceKeys = ["choices", "allow_multiple_selection", "allow_other_choice"]
    choiceQ style = do
      cs <- choices ref (propArr "choices")
      let sel = if (props >>= keyBool "allow_multiple_selection") == Just True then Multiple else Single
          other = [Choice "other" (plain "Other") True False | (props >>= keyBool "allow_other_choice") == Just True]
      pure (QChoice sel style (cs ++ other), choiceKeys, [], [])

choices :: Text -> [Value] -> Conv [Choice]
choices ref cs = do
  when (any (isJust . key "attachment") cs) $ warn ref "choice pictures dropped"
  pure [choice (fromMaybe l (keyText "ref" c)) l | c <- cs, let l = keyTextOr "" "label" c]

readValidation :: Text -> Maybe Value -> Maybe Validation
readValidation ty vals = case (lo, hi) of
  (Nothing, Nothing) -> Nothing
  _ -> Just (Validation lo hi Nothing Nothing)
  where
    num k = vals >>= keyNum k
    (lo, hi)
      | ty == "number" = (num "min_value", num "max_value")
      | ty `elem` ["short_text", "long_text"] = (Nothing, num "max_length")
      | otherwise = (Nothing, Nothing)

-- | Convert forward jumps to relevance; everything else is returned as raw
-- logic blocks for a Typeform round trip.
--
-- After a field is answered its actions are tried in order and the first
-- jump whose condition holds wins, skipping the fields between it and its
-- target. A field is hidden when some jump over it fires first, so its
-- relevance is the negation of the disjunction of those conditions.
readLogic :: M.Map Text QType -> [Text] -> Int -> [Entry] -> [Value] -> Conv (M.Map Text Expr, [Value])
readLogic types hidden endings es blocks = do
  converted <- forM blocks $ \b -> do
    let src = case keyText "type" b of
          Just "hidden" -> Just (-1)
          _ -> keyText "ref" b >>= (`M.lookup` pos)
        at = keyTextOr "typeform" "ref" b
    -- A jump kept raw may fire before the jumps after it, so those are
    -- kept raw too, in order.
    let step blocked a = case convert src a of
          Right j
            | blocked -> (blocked, Left a) <$ warn at "jump after an unconverted jump kept only for typeform"
            | otherwise -> do
                when (endings > 1 && (key "details" a >>= key "to" >>= keyText "type") == Just "thankyou") $
                  warn at "choice of ending screen dropped"
                pure (blocked, Right j)
          Left why -> (blocked || keyText "action" a == Just "jump", Left a) <$ warn at why
    acts <- snd <$> mapAccumM step False (keyArr "actions" b)
    pure ((src, [j | Right j <- acts]), [b' | let raw = [a | Left a <- acts], not (null raw), let b' = withActions b raw])
  let sources = [(s, js) | ((Just s, js), _) <- converted, not (null js)]
      raws = concatMap snd converted
      rels = runBdd posOf $ do
        conds <- forM sources $ \(s, js) -> (,) s <$> forM js (\(c, t) -> (\n -> (n, t)) <$> fromExpr c)
        relAt0 <- foldM (relevance (entered sources) conds) M.empty [0 .. len - 1]
        relAt <- liftShared relAt0
        M.fromList <$> sequence [(,) (eRef e) <$> toExpr r | (n, e) <- zip [0 ..] es, Just r <- [M.lookup n relAt]]
  pure (rels, raws)
  where
    pos = M.fromList [(eRef e, n) | (n, e) <- zip [0 :: Int ..] es]
    len = length es
    withActions b as = case b of
      Object o -> Object (KM.insert "actions" (toJSON as) o)
      x -> x
    convert src a
      | keyText "action" a /= Just "jump" = Left "variable action kept only for typeform"
      | otherwise = do
          s <- maybe (Left "jump from an unknown field kept only for typeform") Right src
          let to = key "details" a >>= key "to"
          t <- case to >>= keyText "type" of
            Just "field" -> maybe (Left "jump to an unknown field kept only for typeform") Right (to >>= keyText "value" >>= (`M.lookup` pos))
            Just ty | ty `elem` ["thankyou", "outcome"] -> Right len
            _ -> Left "jump target kept only for typeform"
          unless (t > s) $ Left "backward jump kept only for typeform"
          c <- maybe (Left "jump condition kept only for typeform") Right (key "condition" a >>= parseCond types)
          pure (c, t)
    -- Whether a jump from s to t hides n as a unit: n lies between them
    -- and no group around it is hidden whole. A group that some jump enters
    -- partway, from outside, is never a unit: its fields carry their own
    -- relevance.
    covers ent s t n = case drop n es of
      (e : _) ->
        s < n && n < t && not (S.member n ent)
          && all (\p -> S.member p ent || not (s < p && maybe False ((<= t) . eEnd) (lookupE p))) (eParents e)
      [] -> False
    entered sources =
      S.fromList
        [ g
        | (s, js) <- sources
        , (_, t) <- js
        , (g, e) <- zip [0 ..] es
        , eGroup e
        , s < g
        , g < t
        , t < eEnd e
        ]
    lookupE p = case drop p es of
      (e : _) -> Just e
      [] -> Nothing
    posOf r = if r `elem` hidden then -1 else M.findWithDefault maxBound r pos
    skips s t n = s < n && n < t
    -- A group whose fields all share one condition shows exactly when that
    -- condition holds; it takes the condition over, as the writer does.
    liftShared relAt =
      foldM
        ( \m (g, e) ->
            let kids = [n | (n, k) <- zip [0 ..] es, take 1 (eParents k) == [g]]
             in case map (`M.lookup` m) kids of
                  (Just r : rs) | eGroup e, all (== Just r) rs -> do
                    r' <- maybe (pure r) (bAnd r) (M.lookup g m)
                    pure (M.insert g r' (foldr M.delete m kids))
                  _ -> pure m
        )
        relAt
        (zip [0 ..] es)
    parentsOf s = maybe [] eParents (lookupE s)
    -- A field is hidden when a jump over it fires: the jump's source is
    -- shown, its condition holds, and no earlier action of the same source
    -- fired first. An earlier action that skips a group around the field
    -- needs no exclusion: it hides the field either way. The relevance is
    -- the negation, in canonical form.
    relevance ent conds relAt n = do
      parts <-
        sequence
          [ do
              negs <- mapM (bNot . fst) [j | j@(_, t') <- take i js, not (skips s t' n)]
              -- Groups around both the source and this field are shown
              -- whenever this field could be: they add nothing.
              let vis = [M.findWithDefault true p relAt | p <- s : parentsOf s, p >= 0, p `notElem` parentsOf n]
              bAnds (c : vis ++ negs)
          | (s, js) <- conds
          , (i, (c, t)) <- zip [0 ..] js
          , covers ent s t n
          ]
      r <- bOrs parts >>= bNot
      pure (if r == true then relAt else M.insert n r relAt)

-- | A Typeform condition tree. Leaves the IR cannot model are kept as raw
-- Typeform JSON.
parseCond :: M.Map Text QType -> Value -> Maybe Expr
parseCond types c = case keyText "op" c of
  Just "always" -> Just (ELit (LBool True))
  Just "and" -> fold And
  Just "or" -> fold Or
  Just op -> Just (fromMaybe raw (leaf op (keyArr "vars" c)))
  Nothing -> Nothing
  where
    raw = ERaw "typeform" (TE.decodeUtf8 (BL.toStrict (encode c)))
    fold o = case mapM (parseCond types) (keyArr "vars" c) of
      Just (x : xs) -> Just (foldl (EBin o) x xs)
      _ -> Nothing
    leaf op [a, b] = do
      r <- if keyText "type" a `elem` [Just "field", Just "hidden"] then keyText "value" a else Nothing
      lit <- case (keyText "type" b, key "value" b) of
        (Just "choice", Just x) -> LStr <$> asText x
        (Just "constant", Just (Number n)) -> Just (LNum (toRealFloat n))
        (Just "constant", Just (String s)) -> Just (LStr s)
        (Just "constant", Just (Bool x)) -> Just (LBool x)
        _ -> Nothing
      let multi = case M.lookup r types of
            Just (QChoice Multiple _ _) -> True
            _ -> False
      case (keyText "type" b == Just "choice" && multi, lit, op) of
        (True, LStr v, _) | op `elem` ["is", "equal"] -> Just (ESelected r v)
        (True, LStr v, _) | op `elem` ["is_not", "not_equal"] -> Just (ENot (ESelected r v))
        _ -> (\o -> EBin o (ERef r) (ELit lit)) <$> lookup op cmpNames
    leaf _ _ = Nothing

cmpNames :: [(Text, BinOp)]
cmpNames =
  [ ("is", Eq)
  , ("equal", Eq)
  , ("is_not", Neq)
  , ("not_equal", Neq)
  , ("lower_than", Lt)
  , ("lower_equal_than", Le)
  , ("greater_than", Gt)
  , ("greater_equal_than", Ge)
  ]

------------------------------------------------------------------------------
-- Writing

-- | A lowered field: ref, relevance, and its JSON given its rendered
-- children (groups only).
data Node = Node Text (Maybe Expr) (Maybe [Node]) ([Value] -> Value)

render :: Node -> Value
render (Node _ _ kids f) = f (maybe [] (map render) kids)

treeOf :: Node -> Tree
treeOf (Node r rel kids _) = Tree r rel (map treeOf <$> kids)

writeTypeform :: Form -> Conv BL.ByteString
writeTypeform s@(Form meta items0) = do
  items <- uniqueRefs items0
  when (hasTranslations s) $ warn "typeform" "translations dropped, a typeform has one language"
  when (isJust (metaDescription meta)) $ warn "typeform" "survey description dropped"
  forM_ (drop 1 (metaLanguages meta)) $ \l -> warn "typeform" ("language " <> l <> " dropped")
  let taken = S.fromList [itemId i | i <- allItems items, not (isGroupItem i)]
      isGroupItem = \case
        ItemGroup _ -> True
        _ -> False
  lowered <- mapM (lower taken True Nothing) items
  let nodes = concatMap fst lowered
      hidden = concatMap snd lowered
      qs = [q | ItemQuestion q <- allItems items]
      types = M.fromList [(qId q, qType q) | q <- qs]
      known = S.fromList (hidden ++ map eRef (flatten (map treeOf nodes)))
      endRef = case M.lookup (attrKey "thankyou_screens") (metaAttrs meta) of
        Just (Array a) | (t : _) <- foldr (:) [] a, Just r <- keyText "ref" t -> r
        _ -> "default_tys"
  -- "Has selected" on a single-choice field reads back as equality.
      single q = case M.lookup q types of
        Just (QChoice Single _ _) -> True
        _ -> False
      asEq = \case
        ESelected q v | single q -> EBin Eq (ERef q) (ELit (LStr v))
        ENot x -> ENot (asEq x)
        EBin o a b -> EBin o (asEq a) (asEq b)
        x -> x
      flat = [e {eRel = asEq <$> eRel e} | e <- flatten (map treeOf nodes)]
      posOf r = if r `elem` hidden then -1 else M.findWithDefault maxBound r (M.fromList (zip (map eRef flat) [0 ..]))
  generated <- writeLogic posOf (toCond types (S.fromList hidden) known) endRef flat
  let raw = case M.lookup (attrKey "logic") (metaAttrs meta) of
        Just (Array a) -> foldr (:) [] a
        _ -> []
      logic = mergeLogic generated raw
      endingFields = case M.lookup (attrKey "ending_fields") (metaAttrs meta) of
        Just (Array a) -> foldr (:) [] a
        _ -> []
      settings = case (M.lookup (attrKey "settings") (metaAttrs meta), metaLanguages meta) of
        (Just (Object o), l : _) -> Just (Object (KM.insert "language" (String l) o))
        (Just x, []) -> Just x
        (_, l : _) -> Just (object ["language" .= l])
        (_, []) -> Nothing
  pure . encodePretty . obj $
    [ ("title" .=?) . String . i18nDefault =<< metaTitle meta
    , ("settings" .=?) =<< settings
    , "fields" .=? toJSON (map render nodes ++ endingFields)
    , if null hidden then Nothing else "hidden" .=? toJSON hidden
    , if null logic then Nothing else "logic" .=? toJSON logic
    ]
      ++ [K.fromText k .=? x | k <- passthrough, Just x <- [M.lookup (attrKey k) (metaAttrs meta)]]

-- | Typeform refs are unique. A field or group whose id repeats an
-- earlier one of its kind gets a numbered ref; logic keeps pointing at the
-- first. (A group that shares a field's id is renamed when lowered.)
uniqueRefs :: [Item] -> Conv [Item]
uniqueRefs items = evalStateT (mapM go items) (S.empty, S.empty)
  where
    ids = S.fromList (map itemId (allItems items))
    go = \case
      ItemGroup g -> do
        i <- fresh snd (\x (f, gs) -> (f, S.insert x gs)) (groupId g)
        kids <- mapM go (groupItems g)
        pure (ItemGroup g {groupId = i, groupItems = kids})
      ItemQuestion q -> (\i -> ItemQuestion q {qId = i}) <$> fresh fst (\x (f, gs) -> (S.insert x f, gs)) (qId q)
      ItemNote i t r -> (\i' -> ItemNote i' t r) <$> fresh fst (\x (f, gs) -> (S.insert x f, gs)) i
      x -> pure x
    fresh sel add i = do
      st <- get
      if S.member i (sel st)
        then do
          let i' = case [c | n <- [2 :: Int ..], let c = i <> "_" <> tshow n, not (S.member c ids), not (S.member c (sel st))] of
                (c : _) -> c
                [] -> i
          lift (warn i ("repeated ref renamed " <> i'))
          put (add i' st)
          pure i'
        else i <$ put (add i st)

-- | Generated blocks in field order; raw actions appended to the block of
-- the same field, or as blocks of their own.
mergeLogic :: [(Text, [Value])] -> [Value] -> [Value]
mergeLogic generated raw =
  [ object ["type" .= ("field" :: Text), "ref" .= r, "actions" .= (as ++ extra r)]
  | (r, as) <- generated
  ]
    ++ [b | b <- raw, not (fieldBlock b `elem` map (Just . fst) generated)]
  where
    fieldBlock b = if keyText "type" b == Just "field" then keyText "ref" b else Nothing
    extra r = concat [keyArr "actions" b | b <- raw, fieldBlock b == Just r]

andRel :: Maybe Expr -> Maybe Expr -> Maybe Expr
andRel Nothing b = b
andRel a Nothing = a
andRel (Just a) (Just b) = Just (EBin And a b)

-- | Lower an item to Typeform fields and hidden-field names. Groups nest
-- one level; deeper groups are flattened and their relevance moves to
-- their children.
lower :: S.Set Text -> Bool -> Maybe Expr -> Item -> Conv ([Node], [Text])
lower taken top inherited = \case
  ItemQuestion q
    | qType q == QHidden -> do
        unless (isNothing (andRel inherited (qRelevant q))) $ warn (qId q) "hidden field logic dropped"
        unless (qLabel q == plain (qId q) && isNothing (qHint q)) $ warn (qId q) "hidden field label dropped, typeform hidden fields are names"
        when (qRequired q || isJust (qDefault q) || isJust (qValidation q)) $ warn (qId q) "hidden field settings dropped"
        pure ([], [qId q])
    | otherwise -> do
        f <- fieldJSON q
        pure ([Node (qId q) (andRel inherited (qRelevant q)) Nothing (const v) | Just v <- [f]], [])
  ItemNote i t rel ->
    pure ([Node i (andRel inherited rel) Nothing (const (object ["ref" .= i, "title" .= i18nDefault t, "type" .= ("statement" :: Text)]))], [])
  ItemRaw (Format "typeform") v -> pure ([Node (keyTextOr "" "ref" v) Nothing Nothing (const v)], [])
  ItemRaw (Format f) _ -> ([], []) <$ warn "typeform" ("raw " <> f <> " content dropped")
  ItemGroup g
    | top -> do
        when (groupKind g == Repeat) $ warn (groupId g) "repeat written as a plain group"
        kids <- mapM (lower taken False Nothing) (groupItems g)
        ref <-
          if S.member (groupId g) taken
            then (groupId g <> "_group") <$ warn (groupId g) "group ref renamed, typeform refs must be unique"
            else pure (groupId g)
        let props = case M.lookup (attrKey "properties") (groupAttrs g) of
              Just (Object o) -> o
              _ -> KM.empty
            build vs =
              obj
                [ "ref" .=? String ref
                , ("title" .=?) . String . i18nDefault =<< groupTitle g
                , "type" .=? "group"
                , "properties" .=? Object (KM.insert "fields" (toJSON vs) props)
                ]
        pure ([Node ref (andRel inherited (groupRelevant g)) (Just (concatMap fst kids)) build], concatMap snd kids)
    | otherwise -> do
        warn (groupId g) "nested group flattened, typeform groups nest one level"
        kids <- mapM (lower taken False (andRel inherited (groupRelevant g))) (groupItems g)
        pure (concatMap fst kids, concatMap snd kids)

-- | Jump rules that hide each field exactly when its relevance fails.
--
-- Consecutive fields with the same hidden-condition form a run; the field
-- before a run jumps past it when the condition holds. When the next run is
-- conditional too, longer jumps come first so the first match lands on the
-- first field that is shown.
writeLogic :: (Text -> Int) -> (Expr -> Maybe Value) -> Text -> [Entry] -> Conv [(Text, [Value])]
writeLogic posOf cond endRef es0 = do
  let (ws, out) = runBdd posOf $ do
        rels <- groupRels es0 =<< mapM (traverse fromExpr . eRel) es0
        hsRaw <- forM (zip es0 rels) $ \(e, rel) -> case rel of
          Nothing -> pure (Nothing, [])
          Just r -> do
            h <- bNot r
            x <- toExpr h
            pure $ case () of
              _
                | h == false -> (Nothing, [])
                | isJust (cond x) -> (Just h, [])
                | otherwise -> (Nothing, [(eRef e, "logic dropped, a typeform jump cannot express it")])
        let hs0 = map fst hsRaw
            bad = [i | i <- runStarts hs0, isNothing (sourceOf i)]
            hs = [if any (\i -> n `elem` runMembers hs0 i) bad then Nothing else h | (n, h) <- zip [0 ..] hs0]
        blocks <- forM (runStarts hs) $ \i -> do
          acts <- forM (reverse (chainFrom hs i)) $ \(cs, t) -> do
            n <- bAnds cs
            c <- toExpr n
            -- A chain step whose conditions contradict each other never
            -- fires; it is left out.
            pure $ case cond c of
              _ | n == false -> Right Nothing
              Just v -> Right (Just (jump v t))
              Nothing -> Left (refAt t, "logic dropped, a typeform jump cannot express it")
          pure ((fromMaybe 0 (sourceOf i), [a | Right (Just a) <- acts]), [w | Left w <- acts])
        -- Runs that share a source (a group and its first field) jump from
        -- it in run order: the group's jumps first.
        let bySource = M.fromListWith (flip (++)) (map fst blocks)
        pure
          ( concatMap snd hsRaw ++ [(refAt i, "logic dropped, no field before it to jump from") | i <- bad] ++ concatMap snd blocks
          , [(refAt src, as) | (src, as) <- M.toList bySource, not (null as)]
          )
  mapM_ (uncurry warn) ws
  pure out
  where
    len = length es0
    at n = case drop n es0 of
      (e : _) -> Just e
      [] -> Nothing
    refAt n = maybe "" eRef (at n)
    end n = maybe (n + 1) eEnd (at n)
    -- The field a run's jumps start from: the one before it, looking past
    -- the starts of groups the run opens.
    sourceOf i = go (i - 1)
      where
        go j
          | j < 0 = Nothing
          | maybe False eGroup (at j) = go (j - 1)
          | otherwise = Just j
    hAt hs n = case drop n hs of
      (h : _) -> h
      [] -> Nothing
    -- Members of the run starting at i, stepping over subtrees.
    runMembers hs i = case hAt hs i of
      Nothing -> []
      Just h -> go i
        where
          go n
            | n < len && hAt hs n == Just h = n : go (end n)
            | otherwise = []
    after hs i = case reverse (runMembers hs i) of
      (m : _) -> end m
      [] -> i
    runStarts hs = go S.empty 0
      where
        go seen n
          | n >= len = []
          | S.member n seen = go seen (n + 1)
          | isJust (hAt hs n) = n : go (S.union seen (S.fromList (runMembers hs n))) (n + 1)
          | otherwise = go seen (n + 1)
    -- Accumulated conditions and the landing index for each run in the
    -- chain that starts at run i.
    chainFrom hs i = go [] i
      where
        go acc n = case hAt hs n of
          Just h | n < len -> let acc' = acc ++ [h]; t = after hs n in (acc', t) : go acc' (next t)
          _ -> []
        -- Landing on a group that is shown goes on to its first field.
        next t = case at t of
          Just e | eGroup e, isNothing (hAt hs t), eEnd e > t + 1 -> next (t + 1)
          _ -> t
    jump c t =
      object
        [ "action" .= ("jump" :: Text)
        , "details" .= object ["to" .= if t >= len then object ["type" .= ("thankyou" :: Text), "value" .= endRef] else object ["type" .= ("field" :: Text), "value" .= refAt t]]
        , "condition" .= c
        ]

-- | Relevance as jumps see it, normalised the way a read-back produces
-- it. A field's condition that its group's
-- already implies is dropped. A group whose first field keeps a condition
-- of its own is entered partway by that field's jumps, so it cannot keep a
-- condition either: its fields take it instead.
groupRels :: [Entry] -> [Maybe Bdd.Node] -> Bdd [Maybe Bdd.Node]
groupRels es relsIn = do
  -- A group whose fields all share one condition takes it over, as a jump
  -- past the whole group reads back.
  let atIn = IM.fromList (zip [0 ..] relsIn)
      kids g = [n | (n, e) <- zip [0 ..] es, take 1 (eParents e) == [g]]
      shared g = case map (\n -> IM.findWithDefault Nothing n atIn) (kids g) of
        (Just r : rs) | all (== Just r) rs -> Just r
        _ -> Nothing
      lifted = IM.fromList [(g, r) | (g, e) <- zip [0 ..] es, eGroup e, Just r <- [shared g]]
  rels0 <- forM (zip [0 ..] (zip es relsIn)) $ \(n, (e, r)) -> case IM.lookup n lifted of
    Just rk -> maybe (pure (Just rk)) (fmap Just . bAnd rk) r
    Nothing
      | any (`IM.member` lifted) (take 1 (eParents e)) -> pure Nothing
      | otherwise -> pure r
  let at = IM.fromList (zip [0 ..] rels0)
      relOf n = IM.findWithDefault Nothing n at
  rels1 <- forM (zip es rels0) $ \(e, r) -> case r of
    Nothing -> pure Nothing
    Just rn -> do
      nr <- bNot rn
      implied <- or <$> mapM (\rp -> (== false) <$> bAnd rp nr) [rp | p <- eParents e, Just rp <- [relOf p]]
      pure (if implied then Nothing else Just rn)
  let at1 = IM.fromList (zip [0 ..] rels1)
      rel1 n = IM.findWithDefault Nothing n at1
      pushed = S.fromList [g | (g, e) <- zip [0 ..] es, eGroup e, isJust (rel1 g), eEnd e > g + 1, isJust (rel1 (g + 1))]
  forM (zip [0 ..] (zip es rels1)) $ \(n, (e, r)) ->
    if S.member n pushed
      then pure Nothing
      else case [rg | g <- eParents e, S.member g pushed, Just rg <- [rel1 g]] of
        [] -> pure r
        rgs -> Just <$> bAnds (maybe [] pure r ++ rgs)

-- | A condition as a Typeform condition tree, when it can be one.
toCond :: M.Map Text QType -> S.Set Text -> S.Set Text -> Expr -> Maybe Value
toCond types hidden known = go
  where
    go e = case e of
      ELit (LBool True) -> Just (object ["op" .= ("always" :: Text), "vars" .= ([] :: [Value])])
      EBin o _ _ | o `elem` [And, Or] -> do
        vs <- mapM go (chain o e)
        pure (object ["op" .= (if o == And then "and" else "or" :: Text), "vars" .= vs])
      ESelected r v -> leaf "is" r (choiceVar v)
      ENot (ESelected r v) -> leaf "is_not" r (choiceVar v)
      EBin op (ERef r) (ELit l) -> do
        name <- case (op, l) of
          (Eq, LNum _) -> Just "equal"
          (Neq, LNum _) -> Just "not_equal"
          (Eq, _) -> Just "is"
          (Neq, _) -> Just "is_not"
          (Lt, LNum _) -> Just "lower_than"
          (Le, LNum _) -> Just "lower_equal_than"
          (Gt, LNum _) -> Just "greater_than"
          (Ge, LNum _) -> Just "greater_equal_than"
          _ -> Nothing
        leaf name r (operand r l)
      ERaw "typeform" src -> case A.decodeStrict (TE.encodeUtf8 src) of
        Just v@(Object _) -> Just v
        _ -> Nothing
      _ -> Nothing
    leaf :: Text -> Text -> Value -> Maybe Value
    leaf op r v
      | S.member r known =
          Just (object ["op" .= op, "vars" .= [object ["type" .= (if S.member r hidden then "hidden" else "field" :: Text), "value" .= r], v]])
      | otherwise = Nothing
    choiceVar v = object ["type" .= ("choice" :: Text), "value" .= v]
    operand r l = case (l, M.lookup r types) of
      (LStr v, Just (QChoice Single _ cs)) | v `elem` map choiceValue cs -> choiceVar v
      (LStr v, _) -> constant (String v)
      (LNum n, _) -> constant (toJSON n)
      (LBool b, _) -> constant (Bool b)
    constant v = object ["type" .= ("constant" :: Text), "value" .= v]

hasTranslations :: Form -> Bool
hasTranslations (Form meta items) =
  any translated (catMaybes [metaTitle meta, metaDescription meta] ++ concatMap texts (allItems items))
  where
    translated = not . M.null . i18nTranslations
    texts = \case
      ItemGroup g -> catMaybes [groupTitle g]
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q -> qLabel q : catMaybes [qHint q] ++ case qType q of
        QChoice _ _ cs -> map choiceLabel cs
        QMatrix _ rs cs -> map choiceLabel (rs ++ cs)
        QRanking cs -> map choiceLabel cs
        _ -> []

fieldJSON :: Question -> Conv (Maybe Value)
fieldJSON q = case qType q of
  QCalculate _ -> Nothing <$ warn i "calculated field dropped, typeform variables are not modelled"
  QOther _ | Just raw <- M.lookup (attrKey "field") (qAttrs q) -> pure (Just raw)
  _ -> do
    unless (isNothing (qDefault q)) $ warn i "default value dropped"
    (ty, props, vals) <- typed q
    forM_ (qValidation q) $ \v -> do
      when (isJust (valExpr v)) $ warn i "validation expression dropped"
      when (isJust (valMessage v)) $ warn i "validation message dropped"
    let props' = foldr (\(k, x) -> KM.insert k x) (objAttr "properties") (props ++ [("description", String (i18nDefault h)) | Just h <- [qHint q]])
        vals' = foldr (\(k, x) -> KM.insert k x) (objAttr "validations") (vals ++ [("required", Bool True) | qRequired q])
    pure . Just . obj $
      [ "ref" .=? String i
      , "title" .=? String (i18nDefault (qLabel q))
      , "type" .=? String ty
      , if KM.null props' then Nothing else "properties" .=? Object props'
      , if KM.null vals' then Nothing else "validations" .=? Object vals'
      ]
        ++ [K.fromText k .=? x | k <- ["attachment", "layout"], Just x <- [M.lookup (attrKey k) (qAttrs q)]]
  where
    i = qId q
    objAttr k = case M.lookup (attrKey k) (qAttrs q) of
      Just (Object o) -> o
      _ -> KM.empty

typed :: Question -> Conv (Text, [(K.Key, Value)], [(K.Key, Value)])
typed q = case qType q of
  QText k -> do
    let ty = case k of
          ShortText -> "short_text"
          LongText -> "long_text"
          Email -> "email"
          Url -> "website"
          Phone -> "phone_number"
        lengthy = k `elem` [ShortText, LongText]
    when (isJust vmin || (not lengthy && isJust vmax)) $ warn i "length limits dropped"
    pure (ty, [], [("max_length", toJSON (round' n)) | lengthy, Just n <- [vmax]])
  QNumber k -> do
    when (k == Decimal) $ warn i "decimal written as a typeform number, read back as an integer"
    when (any (\n -> n /= fromIntegral (round' n)) (catMaybes [vmin, vmax])) $ warn i "range limits rounded to whole numbers"
    pure ("number", [], [("min_value", toJSON (round' n)) | Just n <- [vmin]] ++ [("max_value", toJSON (round' n)) | Just n <- [vmax]])
  QChoice sel style cs -> do
    noLimits
    let dropdown = style == Dropdown && sel == Single
        ty
          | dropdown = "dropdown"
          | tyAttr == Just "picture_choice" = "picture_choice"
          | otherwise = "multiple_choice"
        others = filter choiceOther cs
        regular = if dropdown then cs else filter (not . choiceOther) cs
    when (style == Dropdown && sel == Multiple) $ warn i "multi-select dropdown written as multiple choice"
    when (dropdown && not (null others)) $ warn i "\"other\" written as a plain option, dropdowns have none"
    when (any choiceExclusive cs) $ warn i "exclusive choice written as a plain option"
    unless dropdown $ warnFixedCodes i (Just "other") Nothing cs
    when (not dropdown && any ((/= plain "Other") . choiceLabel) others) $ warn i "custom \"other\" label dropped"
    pure
      ( ty
      , [("choices", toJSON (map choiceJSON regular))]
          ++ [("allow_multiple_selection", Bool True) | sel == Multiple, not dropdown]
          ++ [("allow_other_choice", Bool True) | not dropdown, not (null others)]
      , []
      )
  QMatrix sel rows cols -> do
    noLimits
    let row r =
          object
            [ "ref" .= choiceValue r
            , "title" .= i18nDefault (choiceLabel r)
            , "type" .= ("multiple_choice" :: Text)
            , "properties" .= object (["choices" .= map choiceJSON cols] ++ ["allow_multiple_selection" .= True | sel == Multiple])
            ]
    pure ("matrix", [("fields", toJSON (map row rows))], [])
  QRanking cs -> ("ranking", [("choices", toJSON (map choiceJSON cs))], []) <$ noLimits
  QScale lo hi l r -> do
    noLimits
    let labels = labelsJSON l r
    case tyAttr of
      Just "rating" -> do
        when (lo /= 1) $ warn i "rating starts at 1"
        when (isJust l || isJust r) $ warn i "rating end labels dropped"
        pure ("rating", [("steps", toJSON hi)], [])
      Just "nps" | lo == 0 && hi == 10 -> pure ("nps", labels, [])
      _ -> do
        let start = if lo == 1 then 1 else 0
        when (lo /= start) $ warn i "opinion scale starts at 0 or 1"
        pure ("opinion_scale", [("steps", toJSON (hi - start + 1)), ("start_at_one", Bool (start == 1))] ++ labels, [])
  QBoolean -> (if tyAttr == Just "legal" then "legal" else "yes_no", [], []) <$ noLimits
  QDate k -> do
    unless (k == Date) $ warn i "time of day dropped, typeform dates have no time"
    pure ("date", [], [])
  QFile -> pure ("file_upload", [], [])
  QGeo -> ("short_text", [], []) <$ warn i "geopoint written as short text"
  QOther t -> ("short_text", [], []) <$ warn i ("type " <> t <> " written as short text")
  QHidden -> pure ("short_text", [], [])
  QCalculate _ -> pure ("short_text", [], [])
  where
    i = qId q
    vmin = qValidation q >>= valMin
    vmax = qValidation q >>= valMax
    noLimits = when (isJust vmin || isJust vmax) $ warn i "range limits dropped"
    tyAttr = case M.lookup (attrKey "type") (qAttrs q) of
      Just (String t) -> Just t
      _ -> Nothing
    round' :: Double -> Integer
    round' = round
    labelsJSON l r =
      let o =
            obj
              [ ("left" .=?) . String . i18nDefault =<< l
              , ("right" .=?) . String . i18nDefault =<< r
              , ("center" .=?) =<< M.lookup (attrKey "center_label") (qAttrs q)
              ]
       in [("labels", o) | o /= object []]

choiceJSON :: Choice -> Value
choiceJSON c = object ["ref" .= choiceValue c, "label" .= i18nDefault (choiceLabel c)]
