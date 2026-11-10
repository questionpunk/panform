-- | Google Forms API: the @Form@ resource returned by @forms.get@
-- (@google-forms@), and the @forms.batchUpdate@ request body that builds the
-- same form (@google-forms-batch@). Items are a flat list cut into sections
-- by @pageBreakItem@s; a choice has no code (the label is the stored value);
-- a form has one language; the only logic is a per-option "go to section" on
-- single-choice questions, converted to and from page relevance.
--
-- @forms.create@ accepts nothing but a title, so a form is built by creating
-- an empty one and posting the @google-forms-batch@ output to
-- @forms.batchUpdate@: an @updateFormInfo@, one @createItem@ per item with
-- its @location.index@, then an @updateItem@ per branching question that
-- adds its section jumps once the sections they point at exist. Server state
-- in a @forms.get@ result (@revisionId@, @responderUri@, @linkedSheetId@,
-- @publishSettings@) is not part of the definition and is not read.
module Panform.Format.GoogleForms
  ( readGoogleForms
  , writeGoogleForms
  , readGoogleFormsBatch
  , writeGoogleFormsBatch
  ) where

import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Json

tshow :: Int -> Text
tshow = T.pack . show

attrKey :: Text -> Text
attrKey = ("google-forms:" <>)

------------------------------------------------------------------------------
-- Reading

readGoogleForms :: BL.ByteString -> Conv Form
readGoogleForms bs = do
  v <- eitherDecode bs `orFail` "Google Forms JSON"
  fromForm v

-- | A batchUpdate body replayed onto an empty form: @createItem@ inserts at
-- its index, @updateItem@ replaces, info and settings are merged.
readGoogleFormsBatch :: BL.ByteString -> Conv Form
readGoogleFormsBatch bs = do
  v <- eitherDecode bs `orFail` "Google Forms batchUpdate JSON"
  (info, settings, items) <- foldM step (M.empty, Nothing, []) (keyArr "requests" v)
  fromForm . obj $
    [ "info" .=? toJSON info
    , ("settings" .=?) =<< settings
    , "items" .=? toJSON items
    ]
  where
    step (info, settings, items) r
      | Just u <- key "updateFormInfo" r =
          let new = case key "info" u of
                Just (Object o) -> M.fromList [(K.toText k, x) | (k, x) <- KM.toList o]
                _ -> M.empty
              asMap = \case
                Object o -> M.fromList [(K.toText k, x) | (k, x) <- KM.toList o]
                _ -> M.empty
           in pure $ case keyText "updateMask" u of
                -- Without a mask the given fields are merged, as before.
                Nothing -> (M.union new info, settings, items)
                Just m -> (asMap (applyMask m (toJSON new) (toJSON info)), settings, items)
      | Just u <- key "updateSettings" r =
          pure $ case (keyText "updateMask" u, key "settings" u) of
            (Just m, Just new) -> (info, Just (applyMask m new (fromMaybe (object []) settings)), items)
            _ -> (info, key "settings" u, items)
      | Just c <- key "createItem" r, Just it <- key "item" c =
          let i = maybe (length items) round (key "location" c >>= keyNum "index")
              (a, b) = splitAt i items
           in pure (info, settings, a ++ it : b)
      | Just u <- key "updateItem" r, Just it <- key "item" u = do
          let i = maybe (-1) round (key "location" u >>= keyNum "index") :: Int
              -- Only the masked fields change; the rest of the item stays.
              update old = maybe it (\m -> applyMask m it old) (keyText "updateMask" u)
          unless (i >= 0 && i < length items) $ warn "google-forms-batch" ("updateItem ignored: no item at index " <> tshow i)
          pure (info, settings, [if n == i then update x else x | (n, x) <- zip [0 :: Int ..] items])
      | otherwise = (info, settings, items) <$ warn "google-forms-batch" ("request ignored: " <> T.intercalate ", " (requestNames r))
    requestNames = \case
      Object o -> map K.toText (KM.keys o)
      _ -> []

-- | A field mask applied to an object: each listed path takes its new
-- value, or is cleared when the new object does not have it; every other
-- field keeps its old value. @*@ replaces the whole object.
applyMask :: Text -> Value -> Value -> Value
applyMask mask new old
  | null paths || ["*"] `elem` paths = new
  | otherwise = foldl (\acc p -> set p (get p new) acc) old paths
  where
    paths = filter (not . null) (map (filter (not . T.null) . T.splitOn "." . T.strip) (T.splitOn "," mask))
    get p v = foldl (\m k -> m >>= key k) (Just v) p
    set p x v = case p of
      [] -> fromMaybe Null x
      (k : more) ->
        let o = case v of
              Object m -> m
              _ -> KM.empty
            k' = K.fromText k
         in Object $ case more of
              [] -> maybe (KM.delete k') (KM.insert k') x o
              _ -> case (x, KM.lookup k' o) of
                -- Clearing a field of something that is not there.
                (Nothing, Nothing) -> o
                (_, child) -> KM.insert k' (set more x (fromMaybe (object []) child)) o

fromForm :: Value -> Conv Form
fromForm v = do
  let info = fromMaybe (object []) (key "info" v)
      secs = splitSections (zip [1 ..] (keyArr "items" v))
      ids = M.fromList [(i, n) | (n, (Just pb, _)) <- zip [0 ..] secs, Just i <- [keyText "itemId" pb]]
  rels <- sectionRelevance ids secs
  items <- forM (zip3 [0 :: Int ..] secs rels) $ \(n, (hdr, its), rel) -> do
    kids <- catMaybes <$> mapM readItem its
    pure $ case hdr of
      Nothing -> kids
      Just pb ->
        [ ItemGroup
            Group
              { groupId = keyTextOr ("section" <> tshow n) "itemId" pb
              , groupKind = Page
              , groupTitle = plain <$> nonEmpty (keyText "title" pb)
              , groupRelevant = rel
              , groupItems = kids
              , groupAttrs = M.fromList [(attrKey "description", String d) | Just d <- [nonEmpty (keyText "description" pb)]]
              }
        ]
  let title = nonEmpty (keyText "title" info)
      meta =
        emptyMeta
          { metaId = keyText "formId" v
          , metaTitle = plain <$> title
          , metaDescription = plain <$> nonEmpty (keyText "description" info)
          , metaAttrs =
              M.fromList $
                [(attrKey "documentTitle", String d) | Just d <- [keyText "documentTitle" info], Just d /= title]
                  ++ [(attrKey "settings", s) | Just s@(Object o) <- [key "settings" v], not (KM.null o)]
          }
  pure (Form meta (concat items))

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null t) -> Just t
  _ -> Nothing

-- | Cut the flat item list at page breaks. The first section has no header.
splitSections :: [(Int, Value)] -> [(Maybe Value, [(Int, Value)])]
splitSections = go Nothing []
  where
    go hdr acc [] = [(hdr, reverse acc)]
    go hdr acc (it@(_, x) : rest)
      | isJust (key "pageBreakItem" x) = (hdr, reverse acc) : go (Just x) [] rest
      | otherwise = go hdr (it : acc) rest

-- | Section relevance from option jumps. A jump from section @a@ to section
-- @t@ skips the sections in between, so each of them is shown only when the
-- answer is not one of the options that jump over it.
sectionRelevance :: M.Map Text Int -> [(Maybe Value, [(Int, Value)])] -> Conv [Maybe Expr]
sectionRelevance ids secs = do
  skips <- fmap concat . forM (zip [0 ..] secs) $ \(a, (_, its)) ->
    case reverse (mapMaybe branching its) of
      [] -> pure []
      ((q, opts) : earlier) -> do
        forM_ earlier $ \(q', _) -> warn q' "branching ignored, only the last branching question in a section is converted"
        concat <$> mapM (optionSkips a q) opts
  pure [conj [e | (s', e) <- skips, s' == s] | s <- [0 .. n - 1]]
  where
    n = length secs
    conj [] = Nothing
    conj (e : es) = Just (foldl (EBin And) e es)
    optionSkips a q (val, o) = case (keyText "goToSectionId" o, keyText "goToAction" o) of
      (Just t, _) -> case M.lookup t ids of
        Just ti
          | ti > a -> pure [(s, neq q val) | s <- [a + 1 .. ti - 1]]
          | otherwise -> [] <$ warn q ("backward jump to section " <> t <> " dropped")
        Nothing -> [] <$ warn q ("jump to unknown section " <> t <> " dropped")
      (_, Just "SUBMIT_FORM") -> pure [(s, neq q val) | s <- [a + 1 .. n - 1]]
      (_, Just "RESTART_FORM") -> [] <$ warn q "restart-form jump dropped"
      _ -> pure []
    neq q val = EBin Neq (ERef q) (ELit (LStr val))

-- | A single-choice question with at least one option jump: its id and its
-- options by value.
branching :: (Int, Value) -> Maybe (Text, [(Text, Value)])
branching (n, it) = do
  qv <- key "questionItem" it >>= key "question"
  c <- key "choiceQuestion" qv
  ty <- keyText "type" c
  let opts = keyArr "options" c
  if ty `elem` ["RADIO", "DROP_DOWN"] && any jumps opts
    then Just (questionIdOf n it qv, [(optValue o, o) | o <- opts])
    else Nothing
  where
    jumps o = isJust (key "goToSectionId" o) || keyText "goToAction" o `notElem` [Nothing, Just "NEXT_SECTION", Just "GO_TO_ACTION_UNSPECIFIED"]

questionIdOf :: Int -> Value -> Value -> Text
questionIdOf n it qv = fromMaybe (itemIdOf n it) (keyText "questionId" qv)

itemIdOf :: Int -> Value -> Text
itemIdOf n = keyTextOr ("item" <> tshow n) "itemId"

optValue :: Value -> Text
optValue o
  | keyBool "isOther" o == Just True = "other"
  | otherwise = keyTextOr "" "value" o

optChoice :: Value -> Choice
optChoice o
  | keyBool "isOther" o == Just True = Choice "other" (plain (keyTextOr "Other" "value" o)) True False
  | otherwise = let v = keyTextOr "" "value" o in choice v v

readItem :: (Int, Value) -> Conv (Maybe Item)
readItem (n, it)
  | Just qv <- key "questionItem" it >>= key "question" = Just . ItemQuestion <$> readQuestion n it qv
  | Just g <- key "questionGroupItem" it = Just . ItemQuestion <$> readGrid iid it g
  -- A text item's title and description are the note's first paragraph
  -- and the rest; the writer splits them again at the first blank line.
  | isJust (key "textItem" it) = do
      let title = nonEmpty (keyText "title" it)
          body = T.intercalate "\n\n" (catMaybes [title, nonEmpty (keyText "description" it)])
      when (isNothing title && isJust (nonEmpty (keyText "description" it))) $ warn iid "text item with a description and no title read as a note; it is written back as a title"
      when (maybe False ("\n\n" `T.isInfixOf`) title && isJust (nonEmpty (keyText "description" it))) $ warn iid "text item title holds a blank line; its split from the description is not kept"
      pure (Just (ItemNote iid (plain body) Nothing))
  | isJust (key "imageItem" it) || isJust (key "videoItem" it) =
      Just (ItemRaw (Format "google-forms") it) <$ warn iid "image/video item kept only for google forms"
  | otherwise = Nothing <$ warn iid "unknown item kind skipped"
  where
    iid = itemIdOf n it

baseQuestion :: Text -> Value -> QType -> Question
baseQuestion qid it t =
  (question qid "" t)
    { qLabel = plain (keyTextOr "" "title" it)
    , qHint = plain <$> nonEmpty (keyText "description" it)
    }

readQuestion :: Int -> Value -> Value -> Conv Question
readQuestion n it qv = do
  (t, attrs) <- questionKind qid qv
  let idAttr = [(attrKey "itemId", String i) | Just i <- [keyText "itemId" it], i /= qid]
      grading = [(attrKey "grading", g) | Just g <- [key "grading" qv]]
  pure
    (baseQuestion qid it t)
      { qRequired = keyBool "required" qv == Just True
      , qAttrs = M.fromList (idAttr ++ grading ++ attrs)
      }
  where
    qid = questionIdOf n it qv

questionKind :: Text -> Value -> Conv (QType, [(Text, Value)])
questionKind qid qv
  | Just c <- key "choiceQuestion" qv = do
      let opts = keyArr "options" c
          sel = if keyText "type" c == Just "CHECKBOX" then Multiple else Single
          style = if keyText "type" c == Just "DROP_DOWN" then Dropdown else Radio
      when (any (isJust . key "image") opts) $ warn qid "choice images dropped"
      pure (QChoice sel style (map optChoice opts), shuffle c)
  | Just t <- key "textQuestion" qv =
      pure (QText (if keyBool "paragraph" t == Just True then LongText else ShortText), [])
  | Just s <- key "scaleQuestion" qv =
      pure
        ( QScale
            (maybe 1 round (keyNum "low" s))
            (maybe 5 round (keyNum "high" s))
            (plain <$> nonEmpty (keyText "lowLabel" s))
            (plain <$> nonEmpty (keyText "highLabel" s))
        , []
        )
  | Just r <- key "ratingQuestion" qv =
      pure
        ( QScale 1 (maybe 5 round (keyNum "ratingScaleLevel" r)) Nothing Nothing
        , [(attrKey "iconType", String (keyTextOr "STAR" "iconType" r))]
        )
  | Just d <- key "dateQuestion" qv =
      pure
        ( QDate (if keyBool "includeTime" d == Just True then DateTime else Date)
        , [(attrKey "includeYear", Bool False) | keyBool "includeYear" d /= Just True]
        )
  | Just t <- key "timeQuestion" qv =
      pure (QDate Time, [(attrKey "duration", Bool True) | keyBool "duration" t == Just True])
  | Just f <- key "fileUploadQuestion" qv = pure (QFile, [(attrKey "fileUpload", f)])
  | otherwise = do
      warn qid "unknown question kind kept only for google forms"
      pure (QOther "google-forms", [(attrKey "question", qv)])
  where
    shuffle c = [(attrKey "shuffle", Bool True) | keyBool "shuffle" c == Just True]

readGrid :: Text -> Value -> Value -> Conv Question
readGrid qid it g = do
  let rowsQ = keyArr "questions" g
      cols = fromMaybe (object []) (key "grid" g >>= key "columns")
      rows = [Choice (fromMaybe t (keyText "questionId" r)) (plain t) False False | r <- rowsQ, let t = maybe "" (keyTextOr "" "title") (key "rowQuestion" r)]
      sel = if keyText "type" cols == Just "CHECKBOX" then Multiple else Single
  when (isJust (key "image" g)) $ warn qid "grid image dropped"
  pure
    (baseQuestion qid it (QMatrix sel rows (map optChoice (keyArr "options" cols))))
      { qRequired = any ((== Just True) . keyBool "required") rowsQ
      , qAttrs =
          M.fromList $
            [(attrKey "shuffleQuestions", Bool True) | (key "grid" g >>= keyBool "shuffleQuestions") == Just True]
              ++ [(attrKey "shuffle", Bool True) | keyBool "shuffle" cols == Just True]
      }

------------------------------------------------------------------------------
-- Writing

-- | Where an option jumps.
data Target = ToSection Text | Submit
  deriving (Eq, Show)

type Jumps = M.Map Text (M.Map Text Target)

data Lowered = Lowered
  { lFormId :: Maybe Text
  , lInfo :: Value
  , lSettings :: Maybe Value
  , lItems :: [Value]
  }

writeGoogleForms :: Form -> Conv BL.ByteString
writeGoogleForms s = do
  l <- lowerForm (const (pure True)) s
  pure . encodePretty $
    obj
      [ ("formId" .=?) . String =<< lFormId l
      , "info" .=? lInfo l
      , ("settings" .=?) =<< lSettings l
      , "items" .=? toJSON (lItems l)
      ]

writeGoogleFormsBatch :: Form -> Conv BL.ByteString
writeGoogleFormsBatch s = do
  l <- lowerForm creatable s
  let items = lItems l
  let info = case lInfo l of
        Object o -> Object (KM.delete "documentTitle" o)
        x -> x
      mask = case info of
        Object o -> T.intercalate "," (sort (map K.toText (KM.keys o)))
        _ -> ""
  when (isJust (key "documentTitle" (lInfo l))) $ warn "google-forms-batch" "document title dropped, only forms.create sets it"
  let infoReq = [object ["updateFormInfo" .= object ["info" .= info, "updateMask" .= mask]] | not (T.null mask)]
      settingsReq =
        [ object ["updateSettings" .= object ["settings" .= st, "updateMask" .= T.intercalate "," (sort (map K.toText (KM.keys o)))]]
        | Just st@(Object o) <- [lSettings l]
        ]
      creates = [object ["createItem" .= object ["item" .= mapOptions stripJump it, "location" .= loc i]] | (i, it) <- zip [0 :: Int ..] items]
      updates =
        [ object
            [ "updateItem"
                .= object
                  [ "item" .= it
                  , "location" .= loc i
                  , "updateMask" .= ("questionItem.question.choiceQuestion.options" :: Text)
                  ]
            ]
        | (i, it) <- zip [0 :: Int ..] items
        , any hasJump (options it)
        ]
  pure (encodePretty (object ["requests" .= (infoReq ++ settingsReq ++ creates ++ updates)]))
  where
    creatable it
      | isJust (key "questionItem" it >>= key "question" >>= key "fileUploadQuestion") =
          False <$ warn (keyTextOr "" "itemId" it) "file upload question dropped, the API cannot create one"
      | otherwise = pure True
    loc i = object ["index" .= i]
    stripJump = \case
      Object o -> Object (KM.delete "goToSectionId" (KM.delete "goToAction" o))
      x -> x
    hasJump o = isJust (key "goToSectionId" o) || isJust (key "goToAction" o)
    options it = maybe [] (keyArr "options") (key "questionItem" it >>= key "question" >>= key "choiceQuestion")

-- | Apply a function to every option of an item's choice question.
mapOptions :: (Value -> Value) -> Value -> Value
mapOptions f = at "questionItem" (at "question" (at "choiceQuestion" (at "options" arr)))
  where
    at k g = \case
      Object o | Just x <- KM.lookup k o -> Object (KM.insert k (g x) o)
      x -> x
    arr = \case
      Array a -> Array (V.map f a)
      x -> x

-- | The form with items passing @keep@. Leading pages that end up empty
-- are dropped, and the first page's break is not written: a form opens on
-- its first section without one.
lowerForm :: (Value -> Conv Bool) -> Form -> Conv Lowered
lowerForm keep (Form meta items) = do
  when (hasTranslations (Form meta items)) $ warn "google-forms" "translations dropped, a google form has one language"
  let secs = paginate items
      taken = S.fromList (concat [itemId i : [t | ItemQuestion q <- [i], Just (String t) <- [M.lookup (attrKey "itemId") (qAttrs q)]] | i <- allItems items, not (isGroup i)])
      isGroup = \case
        ItemGroup _ -> True
        _ -> False
  sids <- forM (zip [0 :: Int ..] secs) $ \(n, (mg, _)) -> case mg of
    Nothing -> pure ("section" <> tshow n)
    Just g
      | S.member (groupId g) taken -> (groupId g <> "_section") <$ warn (groupId g) "page id renamed, google forms item ids must be unique"
      | otherwise -> pure (groupId g)
  jumps <- planJumps sids secs
  rendered <- forM (zip secs sids) $ \((mg, kids), i) -> do
    body <- filterM keep . concat =<< mapM (writeItem jumps) kids
    pure (mg, i, body)
  let (leading, rest) = span (\(_, _, b) -> null b) rendered
  forM_ [g | (Just g, _, _) <- leading] $ \g -> warn (groupId g) "empty page dropped"
  vals <- case rest of
    [] -> pure []
    ((mg, _, body) : more) -> do
      forM_ mg $ \g -> do
        when (isJust (groupTitle g)) $ warn (groupId g) "first page title dropped, a google form opens without a section header"
        when (isJust (groupRelevant g)) $ warn (groupId g) "logic on the first page dropped"
      pure (body ++ concat [sectionHeader i mg' : b | (mg', i, b) <- more])
  let info =
        obj
          [ ("title" .=?) . String . i18nDefault =<< metaTitle meta
          , ("description" .=?) . String . i18nDefault =<< metaDescription meta
          , ("documentTitle" .=?) =<< M.lookup (attrKey "documentTitle") (metaAttrs meta)
          ]
  pure (Lowered (metaId meta) info (M.lookup (attrKey "settings") (metaAttrs meta)) vals)

sectionHeader :: Text -> Maybe Group -> Value
sectionHeader i mg =
  obj
    [ "itemId" .=? String i
    , ("title" .=?) . String =<< nonEmpty (i18nDefault <$> (mg >>= groupTitle))
    , ("description" .=?) =<< (mg >>= M.lookup (attrKey "description") . groupAttrs)
    , "pageBreakItem" .=? object []
    ]

-- | Top-level pages as they are; loose items between pages become a page.
paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

-- | Turn page relevance back into option jumps. A page's condition must be
-- a conjunction of answers to single-choice questions on earlier pages;
-- each option then skips the pages whose condition it fails, which must be
-- the pages right after its own.
planJumps :: [Text] -> [(Maybe Group, [Item])] -> Conv Jumps
planJumps sids secs = do
  let singles =
        M.fromList
          [ (qId q, (a, pos, cs, qRequired q))
          | (a, (_, kids)) <- zip [0 :: Int ..] secs
          , (pos, ItemQuestion q) <- zip [0 :: Int ..] (allItems kids)
          , QChoice Single _ cs <- [qType q]
          ]
      nsec = length secs
  skips <- fmap concat . forM (zip3 [0 :: Int ..] secs sids) $ \(s, (mg, _), i) ->
    case mg >>= groupRelevant of
      _ | s == 0 -> pure []
      Nothing -> pure []
      Just rel -> case mapM (constraint singles s) (conjuncts rel) of
        Nothing -> [] <$ warn i "page logic dropped, google forms can only skip sections on single-choice answers from earlier sections"
        Just cs -> do
          forM_ [q | (q, _, True) <- cs, Just (_, _, _, False) <- [M.lookup q singles]] $ \q ->
            warn i ("page is now shown when " <> q <> " is unanswered")
          pure [(q, v, s) | (q, vs, _) <- cs, v <- vs]
  let byQ = M.fromListWith (flip (++)) [(q, [(v, s)]) | (q, v, s) <- skips]
  planned <- fmap catMaybes . forM (M.toList byQ) $ \(q, vss) -> case M.lookup q singles of
    Nothing -> pure Nothing
    Just (a, pos, cs, _) ->
      let perOpt = [(choiceValue c, nub (sort [s | (v, s) <- vss, v == choiceValue c])) | c <- cs]
          target ss = if last' ss + 1 >= nsec then Submit else ToSection (sidAt (last' ss + 1))
          ok = and [ss == [a + 1 .. a + length ss] | (_, ss) <- perOpt]
       in if ok
            then pure (Just (a, pos, q, M.fromList [(v, target ss) | (v, ss) <- perOpt, not (null ss)]))
            else Nothing <$ warn q "branching dropped, the pages it would skip do not follow its own"
  -- One branching question per section: keep the last.
  let bySec = M.fromListWith (++) [(a, [(pos, q, m)]) | (a, pos, q, m) <- planned]
  fmap M.unions . forM (M.elems bySec) $ \qs -> case reverse (sort' qs) of
    [] -> pure M.empty
    ((_, q, m) : earlier) -> do
      forM_ earlier $ \(_, q', _) -> warn q' "branching dropped, a google forms section follows one branching question"
      pure (M.singleton q m)
  where
    sidAt n = fromMaybe ("section" <> tshow n) (lookup n (zip [0 ..] sids))
    last' = foldr max 0
    sort' = sortOn (\(p, _, _) -> p)

-- | A conjunct of a page condition as (question, values that skip the page,
-- whether it was a positive test that an unanswered question now passes).
constraint :: M.Map Text (Int, Int, [Choice], Bool) -> Int -> Expr -> Maybe (Text, [Text], Bool)
constraint singles s e = case e of
  EBin Neq (ERef q) l -> (\v -> (q, [v], False)) <$> (valid q =<< litText l)
  ENot (EBin Eq (ERef q) l) -> (\v -> (q, [v], False)) <$> (valid q =<< litText l)
  EBin Eq (ERef q) l -> do
    v <- valid q =<< litText l
    pure (q, [c | c <- values q, c /= v], True)
  EBin Or _ _ -> do
    eqs <- mapM eqOf (disjuncts e)
    case nub (map fst eqs) of
      [q] -> do
        vs <- mapM (valid q . snd) eqs
        pure (q, [c | c <- values q, c `notElem` vs], True)
      _ -> Nothing
  _ -> Nothing
  where
    earlier q = case M.lookup q singles of
      Just (a, _, _, _) -> a < s
      Nothing -> False
    values q = maybe [] (\(_, _, cs, _) -> map choiceValue cs) (M.lookup q singles)
    valid q v = if earlier q && v `elem` values q then Just v else Nothing
    litText = \case
      ELit (LStr t) -> Just t
      ELit (LNum n) -> asText (toJSON n)
      _ -> Nothing
    eqOf = \case
      EBin Eq (ERef q) l -> (,) q <$> litText l
      _ -> Nothing
    disjuncts = \case
      EBin Or a b -> disjuncts a ++ disjuncts b
      x -> [x]

conjuncts :: Expr -> [Expr]
conjuncts = \case
  EBin And a b -> conjuncts a ++ conjuncts b
  x -> [x]

hasTranslations :: Form -> Bool
hasTranslations (Form meta items) =
  any translated (catMaybes [metaTitle meta, metaDescription meta] ++ concatMap texts (allItems items))
  where
    translated = not . M.null . i18nTranslations
    texts = \case
      ItemGroup g -> catMaybes [groupTitle g]
      ItemNote _ t _ -> [t]
      ItemRaw _ _ -> []
      ItemQuestion q -> qLabel q : catMaybes [qHint q] ++ map choiceLabel (qChoices (qType q))
    qChoices = \case
      QChoice _ _ cs -> cs
      QMatrix _ rs cs -> rs ++ cs
      QRanking cs -> cs
      _ -> []

writeItem :: Jumps -> Item -> Conv [Value]
writeItem jumps = \case
  ItemQuestion q -> maybe [] pure <$> writeQuestion (M.findWithDefault M.empty (qId q) jumps) q
  ItemNote i t rel -> do
    unless (isNothing rel) $ warn i "note logic dropped, google forms only branches between sections"
    let (title, rest) = T.breakOn "\n\n" (i18nDefault t)
    pure [obj ["itemId" .=? String i, "title" .=? String title, ("description" .=?) . String =<< nonEmpty (Just (T.drop 2 rest)), "textItem" .=? object []]]
  ItemGroup g -> do
    warn (groupId g) (if groupKind g == Page then "nested page flattened into its section" else "group flattened into its section")
    unless (isNothing (groupRelevant g)) $ warn (groupId g) "group logic dropped"
    concat <$> mapM (writeItem jumps) (groupItems g)
  ItemRaw (Format "google-forms") v -> pure [v]
  ItemRaw (Format f) _ -> [] <$ warn "google-forms" ("raw " <> f <> " content dropped")

data Body = QBody [(K.Key, Value)] | GridBody Value

writeQuestion :: M.Map Text Target -> Question -> Conv (Maybe Value)
writeQuestion jumps q = do
  unless (isNothing (qRelevant q)) $ warn i "question logic dropped, google forms only branches between sections"
  unless (isNothing (qDefault q)) $ warn i "default value dropped"
  unless (isNothing (qValidation q)) $ warn i "validation dropped"
  body <- questionBody jumps q
  let common =
        [ "itemId" .=? String (maybe i id (textAttr "itemId"))
        , "title" .=? String (i18nDefault (qLabel q))
        , ("description" .=?) . String . i18nDefault =<< qHint q
        ]
  pure $ case body of
    Nothing -> Nothing
    Just (GridBody g) -> Just (obj (common ++ ["questionGroupItem" .=? g]))
    Just (QBody fields) ->
      Just . obj $
        common
          ++ [ "questionItem"
                .=? object
                  [ "question"
                      .= object
                        ( ["questionId" .= i]
                            ++ ["required" .= True | qRequired q]
                            ++ [("grading", g) | Just g <- [M.lookup (attrKey "grading") (qAttrs q)]]
                            ++ fields
                        )
                  ]
             ]
  where
    i = qId q
    textAttr k = case M.lookup (attrKey k) (qAttrs q) of
      Just (String t) -> Just t
      _ -> Nothing

questionBody :: M.Map Text Target -> Question -> Conv (Maybe Body)
questionBody jumps q = case qType q of
  QText k -> do
    when (k `elem` [Email, Url, Phone]) $ warn i (T.toLower (T.pack (show k)) <> " validation dropped")
    pure (qb "textQuestion" (obj [if k == LongText then "paragraph" .=? Bool True else Nothing]))
  QNumber _ -> qb "textQuestion" (object []) <$ warn i "number written as short text"
  QChoice sel style cs -> do
    let ty = case (sel, style) of
          (Multiple, _) -> "CHECKBOX"
          (Single, Dropdown) -> "DROP_DOWN"
          (Single, _) -> "RADIO"
    when (sel == Multiple && style == Dropdown) $ warn i "multi-select dropdown written as checkboxes"
    opts <- options (ty == "DROP_DOWN") cs
    pure (qb "choiceQuestion" (object (["type" .= (ty :: Text), "options" .= opts] ++ shuffle)))
  QMatrix sel rows cols -> do
    opts <- options False cols
    pure . Just . GridBody $
      object
        ( [ "questions" .= [rowQ r | r <- rows]
          , "grid"
              .= object
                ( ["columns" .= object (["type" .= (if sel == Multiple then "CHECKBOX" else "RADIO" :: Text), "options" .= opts] ++ shuffle)]
                    ++ ["shuffleQuestions" .= True | M.lookup (attrKey "shuffleQuestions") (qAttrs q) == Just (Bool True)]
                )
          ]
        )
  QRanking cs -> do
    warn i "ranking written as a grid of ranks"
    let ranks = [plain (tshow n) | n <- [1 .. length cs]]
    pure . Just . GridBody $
      object
        [ "questions" .= [rowQ c | c <- cs]
        , "grid" .= object ["columns" .= object ["type" .= ("RADIO" :: Text), "options" .= [object ["value" .= i18nDefault r] | r <- ranks]]]
        ]
  QScale lo hi l h
    | Just icon <- M.lookup (attrKey "iconType") (qAttrs q) -> do
        when (lo /= 1) $ warn i "rating starts at 1"
        when (isJust l || isJust h) $ warn i "rating end labels dropped"
        pure (qb "ratingQuestion" (object ["ratingScaleLevel" .= hi, "iconType" .= icon]))
    | otherwise -> do
        let lo' = max 0 (min 1 lo)
            hi' = max 2 (min 10 hi)
        when (lo' /= lo || hi' /= hi) $ warn i "scale range clamped to google forms' 0-1 low and 2-10 high"
        pure . qb "scaleQuestion" $
          obj
            [ "low" .=? toJSON lo'
            , "high" .=? toJSON hi'
            , ("lowLabel" .=?) . String . i18nDefault =<< l
            , ("highLabel" .=?) . String . i18nDefault =<< h
            ]
  QDate Time -> pure (qb "timeQuestion" (obj [if flag "duration" then "duration" .=? Bool True else Nothing]))
  QDate k ->
    pure . qb "dateQuestion" $
      obj
        [ if k == DateTime then "includeTime" .=? Bool True else Nothing
        , if M.lookup (attrKey "includeYear") (qAttrs q) == Just (Bool False) then Nothing else "includeYear" .=? Bool True
        ]
  QBoolean -> do
    warn i "yes/no written as a two-option choice"
    pure (qb "choiceQuestion" (object ["type" .= ("RADIO" :: Text), "options" .= [object ["value" .= ("Yes" :: Text)], object ["value" .= ("No" :: Text)]]]))
  QFile -> pure (qb "fileUploadQuestion" (fromMaybe (object []) (M.lookup (attrKey "fileUpload") (qAttrs q))))
  QOther _
    | Just raw <- M.lookup (attrKey "question") (qAttrs q) ->
        pure (Just (QBody [(k, x) | Object o <- [raw], (k, x) <- KM.toList o, k `notElem` ["questionId", "required", "grading"]]))
  QOther t -> qb "textQuestion" (object []) <$ warn i ("type " <> t <> " written as short text")
  QGeo -> qb "textQuestion" (object []) <$ warn i "geopoint written as short text"
  QHidden -> Nothing <$ warn i "hidden field dropped, google forms has none"
  QCalculate _ -> Nothing <$ warn i "calculated field dropped"
  where
    i = qId q
    qb k v = Just (QBody [(k, v)])
    flag k = M.lookup (attrKey k) (qAttrs q) == Just (Bool True)
    shuffle = ["shuffle" .= True | flag "shuffle"]
    rowQ r =
      object $
        ["rowQuestion" .= object ["title" .= i18nDefault (choiceLabel r)]]
          ++ ["questionId" .= choiceValue r | plain (choiceValue r) /= choiceLabel r]
          ++ ["required" .= True | qRequired q]
    options dropdown cs = do
      let (others, regular) = (filter choiceOther cs, filter (not . choiceOther) cs)
      when (any (\c -> choiceValue c /= i18nDefault (choiceLabel c)) regular) $
        warn i "choice codes dropped, google forms records the label"
      when (any choiceExclusive cs) $ warn i "exclusive choice written as a plain option"
      unless dropdown $ warnFixedCodes i (Just "other") Nothing cs
      when (dropdown && not (null others)) $ warn i "\"other\" written as a plain option, dropdowns have none"
      forM_ [c | c <- others, i18nDefault (choiceLabel c) /= "Other", not dropdown] $ \_ -> warn i "custom \"other\" label dropped"
      let opt c
            | choiceOther c && not dropdown = object (["isOther" .= True] ++ jump c)
            | otherwise = object (["value" .= i18nDefault (choiceLabel c)] ++ jump c)
      pure (map opt (regular ++ others))
    jump c = case M.lookup (choiceValue c) jumps of
      Just (ToSection s) -> ["goToSectionId" .= s]
      Just Submit -> ["goToAction" .= ("SUBMIT_FORM" :: Text)]
      Nothing -> []
