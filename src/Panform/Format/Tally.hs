-- | Tally form JSON (@GET /forms/{id}@, @POST /forms@): a flat @blocks[]@
-- list. A question is a @TITLE@ block followed by a run of input blocks
-- sharing a @groupUuid@ (one block per option for choice questions);
-- @PAGE_BREAK@ blocks split pages, and @CONDITIONAL_LOGIC@ blocks show,
-- hide or jump. Options have no codes, only text.
module Panform.Format.Tally (readTally, writeTally) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.List (groupBy, nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Numeric (showHex)
import Panform.AST
import Panform.Conv
import Panform.Expr (pushNot)
import Panform.Expr.Simplify (simplify)
import Panform.Json
import Panform.Jump

------------------------------------------------------------------------------
-- Reading

-- | A run of consecutive blocks sharing a group.
data Run = Run
  { runIndex :: Int
  , runType :: Text
  , runBlocks :: [Value]
  }

-- | What the reader has built so far for one page.
data PageAcc = PageAcc
  { paId :: Text
  , paTitle :: Maybe I18n
  , paItems :: [Item]
  , paBreak :: Maybe Text
  -- ^ Uuid of the page break that opened the page.
  , paAttrs :: Attrs
  }

readTally :: BL.ByteString -> Conv Form
readTally bs = do
  v <- eitherDecode bs `orFail` "Tally JSON"
  let blocks = keyArr "blocks" v
      runs = zipWith (\n bs' -> Run n (typeOf bs') bs') [0 ..] (groupBy ((==) `on` groupOf) blocks)
      typeOf = \case
        (b : _) -> keyTextOr "" "type" b
        [] -> ""
      groupOf b = keyText "groupUuid" b `orElse` keyText "uuid" b
      title = listToMaybe' [t | Run _ "FORM_TITLE" (b : _) <- runs, Just t <- [nonEmpty (payloadText "title" b) `orElse` nonEmpty (payloadText "html" b)]]
  (pages, logic, owners, choiceMap, hidden) <- walk runs
  let pageIndex = M.fromList [(u, i) | (i, p) <- zip [0 ..] pages, Just u <- [paBreak p]]
      hiddenItems = M.fromList [(itemId i, ()) | p <- pages, i <- paItems p, isHiddenItem i]
  (itemRels, pageRules) <- readLogic owners choiceMap hidden pageIndex (length pages) logic
  pageRels <- jumpsToRelevance (\i -> maybe "tally" paId (listToMaybe' (drop i pages))) [(M.findWithDefault [] i pageRules, Nothing) | i <- [0 .. length pages - 1]]
  let setRel it = case M.lookup (itemId it) itemRels of
        Just e -> withRel e it
        Nothing | M.member (itemId it) hiddenItems -> withRel (ELit (LBool False)) it
        Nothing -> it
      items =
        [ ItemGroup (Group (paId p) Page (paTitle p) rel (map (clearHidden . setRel) (paItems p)) (paAttrs p))
        | (p, rel) <- zip pages pageRels
        ]
  forM_' (M.keys hiddenItems) $ \i -> unless (M.member i itemRels) $ warn i "hidden and never shown"
  pure (Form emptyMeta {metaId = keyText "id" v, metaTitle = plain <$> (title `orElse` keyText "name" v)} items)
  where
    orElse a b = maybe b Just a
    forM_' xs f = mapM_ f xs
    withRel e = \case
      ItemQuestion q -> ItemQuestion q {qRelevant = Just e}
      ItemNote i t _ -> ItemNote i t (Just e)
      i -> i
    isHiddenItem = \case
      ItemQuestion q -> M.member "tally:isHidden" (qAttrs q)
      _ -> False
    clearHidden = \case
      ItemQuestion q -> ItemQuestion q {qAttrs = M.delete "tally:isHidden" (qAttrs q)}
      i -> i

listToMaybe' :: [a] -> Maybe a
listToMaybe' = \case
  (x : _) -> Just x
  [] -> Nothing

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null (T.strip t)) -> Just t
  _ -> Nothing

payload :: Value -> Value
payload = fromMaybe Null . key "payload"

payloadText :: Text -> Value -> Maybe Text
payloadText k = keyText k . payload

payloadBool :: Text -> Value -> Bool
payloadBool k b = keyBool k (payload b) == Just True

-- | Pages, logic payloads (with their page), block-uuid owners, option
-- uuids per question, and hidden-field uuids.
walk :: [Run] -> Conv ([PageAcc], [(Int, Value)], M.Map Text Text, M.Map Text [(Text, Text)], M.Map Text Text)
walk = go [] (PageAcc "page1" Nothing [] Nothing M.empty) Nothing [] M.empty M.empty M.empty
  where
    go done cur pending logic owners opts hidden = \case
      [] -> do
        let cur' = flushPending cur pending
        pure (reverse (cur' : done), reverse logic, owners, opts, hidden)
      (r : rest) -> case runType r of
        "FORM_TITLE" -> go done cur pending logic owners opts hidden rest
        "PAGE_BREAK" -> do
          let b = headOr Null (runBlocks r)
              n = length done + 2
              pid = fromMaybe ("page" <> T.pack (show n)) (nonEmpty (payloadText "name" b))
              attrs = M.fromList [("tally:isThankYouPage", Bool True) | payloadBool "isThankYouPage" b]
          go (flushPending cur pending : done) (PageAcc pid Nothing [] (keyText "uuid" b) attrs) Nothing logic owners opts hidden rest
        "CONDITIONAL_LOGIC" ->
          go done cur pending ([(length done, payload b) | b <- runBlocks r] ++ logic) owners opts hidden rest
        "TITLE" -> do
          let cur' = flushPending cur pending
              b = headOr Null (runBlocks r)
          go done cur' (Just (runIndex r, b)) logic owners opts hidden rest
        t
          | t `elem` ["HEADING_1", "HEADING_2", "HEADING_3"]
          , all isHiddenField (paItems cur)
          , Nothing <- pending
          , Nothing <- paTitle cur ->
              go done cur {paTitle = plain <$> payloadText "html" (headOr Null (runBlocks r))} Nothing logic owners opts hidden rest
          | t `elem` ["TEXT", "LABEL", "HEADING_1", "HEADING_2", "HEADING_3"] -> do
              let cur' = flushPending cur pending
                  nid = "note" <> T.pack (show (runIndex r))
                  note = ItemNote nid (plain (fromMaybe "" (payloadText "html" (headOr Null (runBlocks r))))) Nothing
                  owners' = foldr (\b -> maybe id (`M.insert` nid) (keyText "uuid" b)) owners (runBlocks r)
              go done cur' {paItems = paItems cur' ++ [note]} Nothing logic owners' opts hidden rest
          | t == "HIDDEN_FIELDS" -> do
              let fs = concatMap (keyArr "hiddenFields" . payload) (runBlocks r)
                  names = [(u, n) | f <- fs, Just n <- [keyText "name" f], let u = keyTextOr n "uuid" f]
                  qs = [ItemQuestion (question n n QHidden) | (_, n) <- names]
              go done (flushPending cur pending) {paItems = paItems (flushPending cur pending) ++ qs} Nothing logic owners opts (M.union hidden (M.fromList names)) rest
          | Just mk <- inputType t -> do
              let label = pending
                  qid = fromMaybe ("q" <> T.pack (show (runIndex r))) (nonEmpty (listToMaybe' (mapMaybe (payloadText "name") (runBlocks r))))
              q <- mk qid (runBlocks r)
              let q' = q {qLabel = maybe (plain qid) (plain . fromMaybe "" . payloadText "html" . snd) label}
                  uuids = mapMaybe (keyText "uuid") (runBlocks r) ++ mapMaybe (keyText "groupUuid") (runBlocks r) ++ maybe [] (maybe [] pure . keyText "uuid" . snd) label
                  owners' = foldr (`M.insert` qid) owners uuids
                  opts' = M.insert qid [(u, keyTextOr "" "text" (payload b)) | b <- runBlocks r, Just u <- [keyText "uuid" b]] opts
              go done cur {paItems = paItems cur ++ [ItemQuestion q']} Nothing logic owners' opts' hidden rest
          | otherwise -> do
              let cur' = flushPending cur pending
              go done cur' {paItems = paItems cur' ++ [ItemRaw (Format "tally") (toJSON (runBlocks r))]} Nothing logic owners opts hidden rest
    isHiddenField = \case
      ItemQuestion q -> qType q == QHidden
      _ -> False
    flushPending cur = \case
      Nothing -> cur
      Just (n, b) -> cur {paItems = paItems cur ++ [ItemNote ("note" <> T.pack (show n)) (plain (fromMaybe "" (payloadText "html" b))) Nothing]}
    headOr d = \case
      (x : _) -> x
      [] -> d

-- | Payload keys the reader maps into the IR; the rest of the first
-- block's payload is kept as @tally:<key>@ attributes.
consumed :: [Text]
consumed =
  [ "isRequired", "isHidden", "name", "index", "isFirst", "isLast", "text", "allowMultiple", "isOtherOption", "hasOtherOption"
  , "hasMinCharacters", "minCharacters", "hasMaxCharacters", "maxCharacters", "hasMinNumber", "minNumber", "hasMaxNumber"
  , "maxNumber", "start", "end", "hasLeftLabel", "leftLabel", "hasRightLabel", "rightLabel", "stars", "hasDefaultAnswer"
  , "defaultAnswer", "html"
  ]

inputType :: Text -> Maybe (Text -> [Value] -> Conv Question)
inputType t = case t of
  "INPUT_TEXT" -> simple (QText ShortText)
  "TEXTAREA" -> simple (QText LongText)
  "INPUT_NUMBER" -> simple (QNumber Decimal)
  "INPUT_EMAIL" -> simple (QText Email)
  "INPUT_LINK" -> simple (QText Url)
  "INPUT_PHONE_NUMBER" -> simple (QText Phone)
  "INPUT_DATE" -> simple (QDate Date)
  "INPUT_TIME" -> simple (QDate Time)
  "FILE_UPLOAD" -> simple QFile
  "SIGNATURE" -> simple QFile
  "MULTIPLE_CHOICE_OPTION" -> choices (\m -> QChoice (sel m) Radio)
  "CHECKBOX" -> choices (const (QChoice Multiple Radio))
  "DROPDOWN_OPTION" -> choices (\m -> QChoice (sel m) Dropdown)
  "MULTI_SELECT_OPTION" -> choices (const (QChoice Multiple Dropdown))
  "RANKING_OPTION" -> choices (const QRanking)
  "LINEAR_SCALE" -> build $ \qid p _ -> do
    when (maybe 1 round (keyNum "step" p) /= (1 :: Int)) $ warn qid "scale step dropped"
    when (keyBool "hasCenterLabel" p == Just True) $ warn qid "centre label dropped"
    pure $
      QScale
        (maybe 0 round (keyNum "start" p))
        (maybe 10 round (keyNum "end" p))
        (if keyBool "hasLeftLabel" p == Just True then plain <$> keyText "leftLabel" p else Nothing)
        (if keyBool "hasRightLabel" p == Just True then plain <$> keyText "rightLabel" p else Nothing)
  "RATING" -> build $ \_ p _ -> pure (QScale 1 (maybe 5 round (keyNum "stars" p)) Nothing Nothing)
  "MATRIX" -> build $ \_ _ bs ->
    let part ty = [choice x x | b <- bs, keyText "type" b == Just ty, let x = keyTextOr "" "text" (payload b)]
        multi = any (payloadBool "allowMultiple") bs
     in pure (QMatrix (if multi then Multiple else Single) (part "MATRIX_ROW") (part "MATRIX_COLUMN"))
  _ -> Nothing
  where
    sel m = if m then Multiple else Single
    simple qt = build (\_ _ _ -> pure qt)
    choices mk = build $ \_ p bs ->
      pure . mk (keyBool "allowMultiple" p == Just True) $
        [ Choice x (plain x) other False
        | b <- bs
        , let x = keyTextOr "" "text" (payload b)
        , let other = payloadBool "isOtherOption" b
        ]
    build f = Just $ \qid bs -> do
      let p = maybe Null payload (listToMaybe' bs)
      qt <- f qid p bs
      let lengthVal = val "hasMinCharacters" "minCharacters" "hasMaxCharacters" "maxCharacters" p
          numVal = val "hasMinNumber" "minNumber" "hasMaxNumber" "maxNumber" p
          attrKeys = case p of
            Object o -> [K.toText k | k <- KM.keys o, K.toText k `notElem` consumed]
            _ -> []
      pure
        Question
          { qId = qid
          , qLabel = plain qid
          , qHint = Nothing
          , qType = qt
          , qRequired = any (payloadBool "isRequired") bs
          , qRelevant = Nothing
          , qValidation = if t == "INPUT_NUMBER" then numVal else lengthVal
          , qDefault = if keyBool "hasDefaultAnswer" p == Just True then key "defaultAnswer" p >>= asText else Nothing
          , qAttrs =
              M.fromList $
                [("tally:" <> k, x) | k <- attrKeys, Just x <- [key k p]]
                  ++ [("tally:type", String t) | t `elem` ["RATING", "SIGNATURE"]]
                  ++ [("tally:isHidden", Bool True) | any (payloadBool "isHidden") bs]
          }
    val hasLo lo hasHi hi p =
      let a = if keyBool hasLo p == Just True then keyNum lo p else Nothing
          b = if keyBool hasHi p == Just True then keyNum hi p else Nothing
       in if isJust a || isJust b then Just (Validation a b Nothing Nothing) else Nothing

-- | Show/hide conditions per item, and jump rules per page.
readLogic ::
  M.Map Text Text ->
  M.Map Text [(Text, Text)] ->
  M.Map Text Text ->
  M.Map Text Int ->
  Int ->
  [(Int, Value)] ->
  Conv (M.Map Text Expr, M.Map Int [Rule])
readLogic owners opts hidden pageIndex nPages logic = do
  parts <- forM logic $ \(page, p) -> case readConds p of
    Left why -> ([], []) <$ warn "tally" ("logic dropped, " <> why)
    Right e -> do
      acts <- forM (keyArr "actions" p) $ \a -> do
        let ap = payload a
            owned k = nub (mapMaybe (\u -> asText u >>= (`M.lookup` owners)) (keyArr k ap))
        case keyText "type" a of
          Just "SHOW_BLOCKS" -> pure ([(i, (True, e)) | i <- owned "showBlocks"], [])
          Just "HIDE_BLOCKS" -> pure ([(i, (False, e)) | i <- owned "hideBlocks"], [])
          Just "JUMP_TO_PAGE" -> case key "jumpToPage" ap of
            Just (Number n) -> pure ([], [(page, Rule e (round (toRealFloat n :: Double) - 1))])
            Just (String u) | Just k <- M.lookup u pageIndex -> pure ([], [(page, Rule e k)])
            _ -> ([], []) <$ warn "tally" "jump to unknown page dropped"
          Just other -> ([], []) <$ warn "tally" (T.toLower other <> " action dropped")
          Nothing -> ([], []) <$ warn "tally" "logic action without a type dropped"
      pure (concatMap fst acts, concatMap snd acts)
  let shows = M.fromListWith (flip (++)) [(i, [x]) | (ss, _) <- parts, (i, x) <- ss]
      rels = M.mapMaybe (\xs -> conj (maybe [] pure (disj [e | (True, e) <- xs]) ++ [neg e | (False, e) <- xs])) shows
      rules = M.fromListWith (flip (++)) [(pg, [r]) | (_, js) <- parts, (pg, r) <- js, ruleTarget r >= 0, ruleTarget r <= nPages]
  pure (rels, rules)
  where
    readConds p = do
      es <- mapM readConditional (keyArr "conditionals" p)
      maybe (Left "no conditions") Right (combine (keyText "logicalOperator" p) es)
    combine op es = if op == Just "OR" then disj es else conj es
    readConditional c = case keyText "type" c of
      Just "GROUP" -> do
        let p = payload c
        es <- mapM readConditional (keyArr "conditionals" p)
        maybe (Left "empty group") Right (combine (keyText "logicalOperator" p) es)
      _ -> readSingle (payload c)
    readSingle p = do
      f <- maybe (Left "condition without a field") Right (key "field" p)
      q <-
        maybe (Left ("unknown field " <> keyTextOr "" "title" f)) Right $
          (keyText "uuid" f >>= (`M.lookup` hidden))
            `orElse` (keyText "blockGroupUuid" f >>= (`M.lookup` owners))
            `orElse` (keyText "uuid" f >>= (`M.lookup` owners))
      let cmp = keyTextOr "" "comparison" p
          optText u = maybe u id (lookup u (M.findWithDefault [] q opts))
          vals = case key "value" p of
            Just (Array a) -> mapMaybe asText (V.toList a)
            Just (String s) -> [s]
            _ -> []
          lit = case key "value" p of
            Just (Number n) -> Just (ELit (LNum (toRealFloat n)))
            Just (String s) -> Just (ELit (LStr (optText s)))
            _ -> Nothing
          many f comb = maybe (Left (cmp <> " without values")) Right (comb (map (f . optText) vals))
          one op = maybe (Left (cmp <> " without a value")) (Right . EBin op (ERef q)) lit
      case cmp of
        "IS" -> one Eq
        "EQUAL" -> one Eq
        "IS_NOT" -> one Neq
        "NOT_EQUAL" -> one Neq
        "GREATER_THAN" -> one Gt
        "LESS_THAN" -> one Lt
        "GREATER_OR_EQUAL_THAN" -> one Ge
        "LESS_OR_EQUAL_THAN" -> one Le
        "IS_ANY_OF" -> many (ESelected q) disj
        "IS_EVERY_OF" -> many (ESelected q) conj
        "IS_NOT_ANY_OF" -> many (ENot . ESelected q) conj
        "IS_EMPTY" -> Right (ENot (EAnswered q))
        "IS_NOT_EMPTY" -> Right (EAnswered q)
        _ -> Right (ECall cmp (ERef q : maybe [] pure lit))
    orElse a b = maybe b Just a

------------------------------------------------------------------------------
-- Writing

-- | A deterministic, UUID-shaped id: namespace, item number, sub-index.
uuid :: Int -> Int -> Int -> Text
uuid ns k j = T.pack (pad 8 (showHex k "") <> "-0000-4000-" <> show (8000 + ns) <> "-" <> pad 12 (showHex j ""))
  where
    pad n s = replicate (n - length s) '0' <> s

-- | A question's input blocks share this group uuid.
groupUuid :: Int -> Text
groupUuid k = uuid 0 k 1

-- | The uuid of option @i@ (from 0) of question @k@.
optionUuid :: Int -> Int -> Text
optionUuid k i = uuid 0 k (1 + i)

hiddenBlock :: Text
hiddenBlock = uuid 3 0 0

-- | What conditions need to know about a question.
data Ref = Ref
  { refK :: Int
  , refType :: QType
  , refBlock :: Text
  , refTitle :: Text
  }

writeTally :: Form -> Conv BL.ByteString
writeTally s@(Form meta items) = do
  when (hasTranslations s) $ warn "tally" "translations dropped, Tally forms have one language"
  when (isJust (metaDescription meta)) $ warn "tally" "survey description dropped"
  sequence_ [warn (qId q) "hidden field label dropped" | ItemQuestion q <- allItems items, qType q == QHidden, qLabel q /= plain (qId q)]
  laid <- layout items
  let (kept, dropped) = partitionOn (either (const True) writable . fst) laid
  mapM_ (either (const (pure [])) (itemBlocks 0) . fst) dropped
  let hidden = [qId q | ItemQuestion q <- allItems items, qType q == QHidden]
      numbered = uniqueNames (S.fromList hidden) (zip [1 ..] kept)
      -- Logic names questions by their old names; the first of a repeated
      -- name keeps it.
      refs =
        M.fromListWith (\_ first -> first) $
          [ (qId q0, Ref k (qType q) (maybe "INPUT_TEXT" fst (inputBlocks q)) (i18nDefault (qLabel q)))
          | ((Right (ItemQuestion q0), _), (k, (Right (ItemQuestion q), _))) <- zip kept numbered
          , not (T.null (qId q0))
          , qId q0 `notElem` hidden
          ]
            ++ [(h, Ref i QHidden "HIDDEN_FIELDS" h) | (i, h) <- zip [1 ..] hidden]
  sequence_
    [ warn (if T.null (qId q) then "tally" else qId q) ("question name written as " <> qId q')
    | ((_, (Right (ItemQuestion q), _)), (_, (Right (ItemQuestion q'), _))) <- zip (zip [1 :: Int ..] kept) numbered
    , qId q /= qId q'
    ]
  pageBlocks <- forM numbered $ \(k, (x, rel)) -> case x of
    Left blocks -> pure (map ($ k) blocks)
    Right it -> do
      body <- itemBlocks k it
      let own = map (keyTextOr "" "uuid") body
      logic <- case rel of
        Nothing -> pure []
        Just e -> case conditionals refs k (simplify e) of
          Left why -> [] <$ warn (itemId it) ("logic dropped, tally cannot express " <> why)
          Right (op, cs) ->
            pure
              [ block (uuid 1 k 0) (uuid 1 k 0) "CONDITIONAL_LOGIC" "CONDITIONAL_LOGIC" $
                  object
                    [ "logicalOperator" .= (op :: Text)
                    , "conditionals" .= cs
                    , "actions" .= [object ["uuid" .= uuid 1 k 1, "type" .= ("SHOW_BLOCKS" :: Text), "payload" .= object ["showBlocks" .= own]]]
                    ]
              ]
      pure (logic ++ (if null logic then body else map hide body))
  let titleBlock = block (uuid 2 0 0) (uuid 2 0 0) "FORM_TITLE" "FORM_TITLE" (obj [("title" .=?) . String . i18nDefault =<< metaTitle meta, ("html" .=?) . String . i18nDefault =<< metaTitle meta])
      hiddenBlocks =
        [ block hiddenBlock hiddenBlock "HIDDEN_FIELDS" "HIDDEN_FIELDS" (object ["hiddenFields" .= [object ["uuid" .= uuid 3 0 (i + 1), "name" .= h] | (i, h) <- zip [0 ..] hidden]])
        | not (null hidden)
        ]
  pure . encodePretty $ object ["status" .= ("DRAFT" :: Text), "blocks" .= (titleBlock : hiddenBlocks ++ concat pageBlocks)]
  where
    hide b = case b of
      Object o | Just (Object p) <- KM.lookup "payload" o -> Object (KM.insert "payload" (Object (KM.insert "isHidden" (Bool True) p)) o)
      _ -> b
    hasTranslations (Form m is) = any tr (catMaybes [metaTitle m]) || any itemTr (allItems is)
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

partitionOn :: (a -> Bool) -> [a] -> ([a], [a])
partitionOn p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])

-- | Whether 'itemBlocks' emits anything; items that are dropped take no
-- number, so uuids stay stable across a round trip.
writable :: Item -> Bool
writable = \case
  ItemNote {} -> True
  ItemRaw (Format "tally") (Array _) -> True
  ItemQuestion q -> isJust (inputBlocks q)
  _ -> False

-- | Question names must be present and unique: an empty or repeated one
-- becomes @q<k>@, or @q<k>_2@ and so on when another question has that
-- name.
uniqueNames :: S.Set Text -> [(Int, (Either a Item, r))] -> [(Int, (Either a Item, r))]
uniqueNames reserved xs = go reserved xs
  where
    claimed = S.fromList [qId q | (_, (Right (ItemQuestion q), _)) <- xs]
    go _ [] = []
    go seen ((k, (Right (ItemQuestion q), r)) : rest) =
      let base = "q" <> T.pack (show k)
          free c = S.notMember c seen && S.notMember c claimed
          n
            | not (T.null (qId q)) && S.notMember (qId q) seen = qId q
            | otherwise = case filter free (base : [base <> "_" <> T.pack (show j) | j <- [2 :: Int ..]]) of
                (c : _) -> c
                [] -> base
       in (k, (Right (ItemQuestion q {qId = n}), r)) : go (S.insert n seen) rest
    go seen (x : rest) = x : go seen rest

block :: Text -> Text -> Text -> Text -> Value -> Value
block u g ty gt p = object ["uuid" .= u, "groupUuid" .= g, "type" .= ty, "groupType" .= gt, "payload" .= p]

-- | Items in order, each with the page break or heading blocks that
-- precede it and the condition it is shown under. Page and group
-- conditions are pushed onto their members.
layout :: [Item] -> Conv [(Either [Int -> Value] Item, Maybe Expr)]
layout items = concat <$> mapM page (zip [1 :: Int ..] (paginate (filter (not . isHidden) items)))
  where
    lastPage = length (paginate (filter (not . isHidden) items))
    isHidden = \case
      ItemQuestion q -> qType q == QHidden
      _ -> False
    page (n, (g, kids)) = do
      let gid = maybe ("page" <> T.pack (show n)) groupId g
          attrs = maybe M.empty groupAttrs g
      when (n == 1 && gid /= "page1") $ warn gid "first page id not carried"
      when (isJust (g >>= groupRelevant)) $ warn gid "page condition copied onto its fields"
      body <- concat <$> mapM (flatten (g >>= groupRelevant)) [k | k <- kids, not (isHidden k)]
      let brk =
            [ \k ->
                block (uuid 4 k 0) (uuid 4 k 0) "PAGE_BREAK" "PAGE_BREAK" . obj $
                  [ "index" .=? toJSON (n - 2)
                  , "isFirst" .=? Bool (n == 2)
                  , "isLast" .=? Bool (n == lastPage)
                  , "name" .=? String gid
                  ]
                    ++ [("isThankYouPage" .=?) x | Just x <- [M.lookup "tally:isThankYouPage" attrs]]
            | n > 1
            ]
          hdr = [\k -> block (uuid 4 k 1) (uuid 4 k 1) "HEADING_2" "HEADING_2" (object ["html" .= i18nDefault t]) | Just t <- [g >>= groupTitle]]
          marks = brk ++ hdr
      pure ([(Left marks, Nothing) | not (null marks)] ++ body)
    flatten rel = \case
      ItemGroup g -> do
        warn (groupId g) "group flattened into its page"
        let rel' = conj (catMaybes [rel, groupRelevant g])
            hdr = [(Right (ItemNote (groupId g) t Nothing), rel') | Just t <- [groupTitle g]]
        (hdr ++) . concat <$> mapM (flatten rel') (groupItems g)
      i -> pure [(Right i, conj (catMaybes [rel, own i]))]
    own = \case
      ItemQuestion q -> qRelevant q
      ItemNote _ _ r -> r
      _ -> Nothing

paginate :: [Item] -> [(Maybe Group, [Item])]
paginate = go []
  where
    go acc [] = flush acc []
    go acc (ItemGroup g : rest) | groupKind g == Page = flush acc ((Just g, groupItems g) : go [] rest)
    go acc (i : rest) = go (i : acc) rest
    flush [] r = r
    flush acc r = (Nothing, reverse acc) : r

-- | The input block type of a question and its option texts, if Tally has
-- a block for it.
inputBlocks :: Question -> Maybe (Text, [Choice])
inputBlocks q = case qType q of
  QChoice _ _ [] -> Just ("INPUT_TEXT", [])
  QRanking [] -> Just ("INPUT_TEXT", [])
  QText ShortText -> Just ("INPUT_TEXT", [])
  QText LongText -> Just ("TEXTAREA", [])
  QText Email -> Just ("INPUT_EMAIL", [])
  QText Url -> Just ("INPUT_LINK", [])
  QText Phone -> Just ("INPUT_PHONE_NUMBER", [])
  QNumber _ -> Just ("INPUT_NUMBER", [])
  QDate Time -> Just ("INPUT_TIME", [])
  QDate _ -> Just ("INPUT_DATE", [])
  QFile | M.lookup "tally:type" (qAttrs q) == Just "SIGNATURE" -> Just ("SIGNATURE", [])
  QFile -> Just ("FILE_UPLOAD", [])
  QChoice Single Dropdown cs -> Just ("DROPDOWN_OPTION", cs)
  QChoice Multiple Dropdown cs -> Just ("MULTI_SELECT_OPTION", cs)
  QChoice Single _ cs -> Just ("MULTIPLE_CHOICE_OPTION", cs)
  QChoice Multiple _ cs -> Just ("CHECKBOX", cs)
  QRanking cs -> Just ("RANKING_OPTION", cs)
  QScale lo _ _ _ | M.lookup "tally:type" (qAttrs q) == Just "RATING", lo == 1 -> Just ("RATING", [])
  QScale {} -> Just ("LINEAR_SCALE", [])
  QMatrix {} -> Just ("MATRIX", [])
  QBoolean -> Just ("MULTIPLE_CHOICE_OPTION", [choice "Yes" "Yes", choice "No" "No"])
  QGeo -> Just ("INPUT_TEXT", [])
  _ -> Nothing

itemBlocks :: Int -> Item -> Conv [Value]
itemBlocks k = \case
  ItemNote i t _ -> do
    warn i "note id not carried"
    pure [block (uuid 0 k 0) (uuid 0 k 0) "TEXT" "TEXT" (object ["html" .= i18nDefault t])]
  ItemRaw (Format "tally") (Array a) -> pure (V.toList a)
  ItemRaw (Format f) _ -> [] <$ warn "tally" ("raw " <> f <> " content dropped")
  ItemGroup g -> [] <$ warn (groupId g) "group dropped"
  ItemQuestion q -> case inputBlocks q of
    Nothing -> [] <$ warn (qId q) (case qType q of QCalculate _ -> "calculated value dropped"; QOther t -> "type " <> t <> " dropped"; _ -> "question dropped")
    Just (ty, cs) -> do
      when (isJust (qHint q)) $ warn (qId q) "hint dropped"
      when (isJust (qValidation q >>= valExpr)) $ warn (qId q) "validation expression dropped"
      when (isJust (qValidation q >>= valMessage)) $ warn (qId q) "validation message dropped"
      case qType q of
        QBoolean -> warn (qId q) "yes/no written as a two-option choice"
        QGeo -> warn (qId q) "geopoint written as a text box"
        QNumber Integer -> warn (qId q) "integer constraint dropped"
        QDate DateTime -> warn (qId q) "time part of the date dropped"
        QChoice _ _ [] -> warn (qId q) "choice question without options written as a text box"
        QRanking [] -> warn (qId q) "ranking without options written as a text box"
        _ -> pure ()
      when (any (\c -> choiceValue c /= i18nDefault (choiceLabel c)) (choicesOf (qType q))) $
        warn (qId q) "choice codes replaced by labels"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive choice written as a plain choice"
      input <- inputs ty cs
      pure (block (uuid 0 k 0) (uuid 0 k 0) "TITLE" "QUESTION" (object ["html" .= i18nDefault (qLabel q)]) : input)
    where
      g = groupUuid k
      common =
        [("isRequired", Bool (qRequired q)), ("name", String (qId q))]
          ++ [(k', x) | (key', x) <- M.toList (qAttrs q), Just k' <- [T.stripPrefix "tally:" key'], k' /= "type"]
      defaults = case qDefault q of
        Just d -> [("hasDefaultAnswer", Bool True), ("defaultAnswer", String d)]
        Nothing -> []
      one ty extra = pure [block g g ty ty (object [(K.fromText a, b) | (a, b) <- common ++ defaults ++ extra])]
      inputs ty cs = case qType q of
        QMatrix sel rows cols ->
          let part bty xs off =
                [ block (uuid 0 k (off + i)) g bty "MATRIX" . object $
                    [ "index" .= i, "isFirst" .= (i == 0), "isLast" .= (i == length xs - 1), "text" .= i18nDefault (choiceLabel c)]
                      ++ ["allowMultiple" .= True | sel == Multiple]
                | (i, c) <- zip [0 ..] xs
                ]
           in pure (block g g "MATRIX" "MATRIX" (object [(K.fromText a, b) | (a, b) <- common]) : part "MATRIX_ROW" rows 100 ++ part "MATRIX_COLUMN" cols 200)
        QScale lo hi l h
          | ty == "RATING" -> one ty [("stars", toJSON hi)]
          | otherwise -> do
              unless (lo >= 0 && hi <= 100) $ warn (qId q) "scale outside 0-100"
              one ty $
                [("start", toJSON lo), ("end", toJSON hi), ("step", toJSON (1 :: Int))]
                  ++ concat [[("hasLeftLabel", Bool True), ("leftLabel", String (i18nDefault x))] | Just x <- [l]]
                  ++ concat [[("hasRightLabel", Bool True), ("rightLabel", String (i18nDefault x))] | Just x <- [h]]
        QText _ -> one ty (limits "Characters")
        QNumber _ -> one ty (limits "Number")
        _
          | null cs -> one ty []
          | otherwise ->
              pure
                [ block (optionUuid k i) g ty (groupTypeOf ty) . object $
                    [(K.fromText a, b) | (a, b) <- common]
                      ++ [ "index" .= i
                         , "isFirst" .= (i == 0)
                         , "isLast" .= (i == length cs - 1)
                         , "text" .= i18nDefault (choiceLabel c)
                         ]
                      ++ ["isOtherOption" .= True | choiceOther c]
                      ++ ["allowMultiple" .= True | i == 0, ty `elem` ["MULTIPLE_CHOICE_OPTION", "DROPDOWN_OPTION"], isMulti]
                | (i, c) <- zip [0 ..] cs
                ]
      isMulti = case qType q of
        QChoice Multiple _ _ -> True
        _ -> False
      limits what = case qValidation q of
        Nothing -> []
        Just v ->
          concat [[("hasMin" <> what, Bool True), ("min" <> what, toJSON x)] | Just x <- [valMin v]]
            ++ concat [[("hasMax" <> what, Bool True), ("max" <> what, toJSON x)] | Just x <- [valMax v]]
      choicesOf = \case
        QChoice _ _ xs -> xs
        QRanking xs -> xs
        QMatrix _ rs xs -> rs ++ xs
        _ -> []

groupTypeOf :: Text -> Text
groupTypeOf = \case
  "MULTIPLE_CHOICE_OPTION" -> "MULTIPLE_CHOICE"
  "CHECKBOX" -> "CHECKBOXES"
  "DROPDOWN_OPTION" -> "DROPDOWN"
  "RANKING_OPTION" -> "RANKING"
  "MULTI_SELECT_OPTION" -> "MULTI_SELECT"
  t -> t

-- | A condition as a top-level operator and Tally conditionals.
conditionals :: M.Map Text Ref -> Int -> Expr -> Either Text (Text, [Value])
conditionals refs k e = case e of
  EBin op _ _ | op `elem` [And, Or] -> (opName op,) <$> mapM (uncurry cond) (zip [0 ..] (flatten op e))
  ENot x | Just y@(EBin op _ _) <- pushNot x, op `elem` [And, Or] -> conditionals refs k y
  _ -> ("AND",) . pure <$> cond 0 e
  where
    opName :: BinOp -> Text
    opName op = if op == And then "AND" else "OR"
    flatten op x = case x of
      EBin o a b | o == op -> flatten op a ++ flatten op b
      _ -> [x]
    cid :: Int -> Text
    cid i = uuid 5 k i
    cond i x = case x of
      EBin op _ _ | op `elem` [And, Or] -> do
        cs <- mapM (\(j, y) -> cond (i * 16 + j + 1) y) (zip [0 ..] (flatten op x))
        pure (object ["uuid" .= cid i, "type" .= ("GROUP" :: Text), "payload" .= object ["logicalOperator" .= opName op, "conditionals" .= cs]])
      EBin op (ERef q) (ELit l) -> case (op, l) of
        (Eq, LStr v) -> single i q "IS" (String (opt q v))
        (Neq, LStr v) -> single i q "IS_NOT" (String (opt q v))
        (Eq, LNum n) -> single i q "EQUAL" (numJSON n)
        (Neq, LNum n) -> single i q "NOT_EQUAL" (numJSON n)
        (Gt, LNum n) -> single i q "GREATER_THAN" (numJSON n)
        (Lt, LNum n) -> single i q "LESS_THAN" (numJSON n)
        (Ge, LNum n) -> single i q "GREATER_OR_EQUAL_THAN" (numJSON n)
        (Le, LNum n) -> single i q "LESS_OR_EQUAL_THAN" (numJSON n)
        _ -> Left ("comparison " <> T.pack (show op) <> " on " <> q)
      ESelected q v -> single i q "IS_ANY_OF" (toJSON [opt q v])
      ENot (ESelected q v) -> single i q "IS_NOT_ANY_OF" (toJSON [opt q v])
      EAnswered q -> single i q "IS_NOT_EMPTY" Null
      ENot (EAnswered q) -> single i q "IS_EMPTY" Null
      ECall op [ERef q] -> single i q op Null
      ECall op [ERef q, ELit (LStr v)] -> single i q op (String v)
      ECall op [ERef q, ELit (LNum n)] -> single i q op (numJSON n)
      ENot y -> maybe (Left "negation") (cond i) (pushNot y)
      _ -> Left (T.pack (take 60 (show x)))
    single i q cmp v = do
      r <- maybe (Left ("reference to " <> q)) Right (M.lookup q refs)
      let field
            | refType r == QHidden =
                object ["uuid" .= uuid 3 0 (refK r), "type" .= ("HIDDEN_FIELD" :: Text), "questionType" .= ("HIDDEN_FIELDS" :: Text), "blockGroupUuid" .= hiddenBlock, "title" .= q]
            | otherwise =
                object ["uuid" .= groupUuid (refK r), "type" .= ("INPUT_FIELD" :: Text), "questionType" .= refBlock r, "blockGroupUuid" .= groupUuid (refK r), "title" .= refTitle r]
      pure (object ["uuid" .= cid i, "type" .= ("SINGLE" :: Text), "payload" .= object ["field" .= field, "comparison" .= (cmp :: Text), "value" .= v]])
    -- A choice value refers to its option block.
    opt q v = case M.lookup q refs of
      Just r
        | Just i <- lookupIndex v (optionTexts (refType r)) -> optionUuid (refK r) i
      _ -> v
    optionTexts = \case
      QChoice _ _ cs -> map choiceValue cs
      QRanking cs -> map choiceValue cs
      QBoolean -> ["Yes", "No"]
      _ -> []
    lookupIndex v xs = lookup v (zip xs [0 ..])
    numJSON n
      | n == fromIntegral (round n :: Integer) = toJSON (round n :: Integer)
      | otherwise = toJSON n
