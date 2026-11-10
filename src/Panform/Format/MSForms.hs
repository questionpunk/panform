-- | Microsoft Forms, in the JSON its web editor exchanges with Microsoft's
-- undocumented internal API (@forms.office.com/formapi/api/{tenant}/users/
-- {user}/forms('{id}')@). Sections are @Question.ColumnGroup@ entries in
-- @descriptiveQuestions@; a numeric @order@ places everything; per-type
-- settings sit in @questionInfo@, a JSON object encoded as a string; logic is
-- "this answer jumps to that question or section" (@BranchInfo@).
--
-- The writer emits that same shape, for tools that POST to the formapi
-- endpoints; Microsoft does not document it and the Forms UI cannot import
-- it. Choices carry only their text, so choice values become their labels.
module Panform.Format.MSForms (readMSForms, writeMSForms) where

import Control.Monad (foldM, unless, when)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (intersect, nub, sortOn, union, (\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Design (readDesign, writeDesign)
import Panform.Expr (pushNot)
import Panform.Json

------------------------------------------------------------------------------
-- Branching, shared by the reader and the writer
--
-- Forms only jumps forward: answering choice @c@ of question @i@ goes to
-- entry @t@, skipping everything strictly between. So an entry @k@ after @i@
-- is shown unless @i@ was answered with a choice that jumps past it, which
-- is the show-if @i != c1 and i != c2 ...@. That is exact whatever other
-- jumps exist, and it is the canonical form both directions produce.

-- | One section header or question, in form order.
data Entry = Entry
  { eId :: Text
  , eHeader :: Bool
  , eLabels :: [Text]
  -- ^ Choice texts, for a single-select question that can branch.
  , eBranches :: [(Text, Target)]
  }

data Target = ToId Text | ToEnd
  deriving (Eq)

-- | The choices of an earlier question that skip an entry.
type Term = (Text, [Text])

-- | Skip terms per entry position, plus jumps that could not be placed.
derive :: M.Map Text Int -> [Entry] -> (M.Map Int [Term], [(Text, Text)])
derive pos es = (M.map collect skips, problems)
  where
    n = length es
    placed =
      [ (e, i, c, maybe (Left x) Right (resolve tgt))
      | (i, e) <- zip [0 ..] es
      , (c, tgt) <- eBranches e
      , let x = case tgt of ToId t -> t; ToEnd -> ""
      ]
    resolve ToEnd = Just n
    resolve (ToId x) = M.lookup x pos
    problems =
      [(eId e, "jump to unknown " <> x <> " dropped") | (e, _, _, Left x) <- placed]
        ++ [(eId e, "backward jump dropped; show-ifs from the other jumps may over-hide") | (e, i, _, Right t) <- placed, t <= i]
    skips = M.fromListWith (flip (++)) [(k, [(i, c)]) | (_, i, c, Right t) <- placed, k <- [i + 1 .. t - 1]]
    collect ics =
      [ (eId src, [c | c <- eLabels src, (i, c) `elem` ics])
      | (i, src) <- zip [0 ..] es
      , i `elem` map fst ics
      ]

-- | Show-if per entry: a section header gets the terms every entry in it
-- shares; the entries keep only what their section does not already say.
regroup :: [Entry] -> M.Map Int [Term] -> [Maybe Expr]
regroup es ts = map rel idx
  where
    idx = [0 .. length es - 1]
    heads = [i | (i, e) <- zip idx es, eHeader e]
    owner j = case filter (<= j) heads of
      [] -> Nothing
      hs -> Just (last hs)
    termsAt j = M.findWithDefault [] j ts
    skipped j src c = maybe False (c `elem`) (lookup src (termsAt j))
    members h = [j | j <- idx, j > h, owner j == Just h]
    groupTerms h =
      filter (not . null . snd) [(src, [c | c <- ls, all (\j -> skipped j src c) (members h)]) | (src, ls) <- termsAt h]
    rel j = case owner j of
      Just h | h == j -> termsExpr (groupTerms h)
      Just h -> termsExpr [t | t@(src, ls) <- termsAt j, lookup src (groupTerms h) /= Just ls]
      Nothing -> termsExpr (termsAt j)

termsExpr :: [Term] -> Maybe Expr
termsExpr ts = ands [EBin Neq (ERef src) (ELit (LStr c)) | (src, cs) <- ts, c <- cs]

ands :: [Expr] -> Maybe Expr
ands [] = Nothing
ands (x : xs) = Just (foldl (EBin And) x xs)

------------------------------------------------------------------------------
-- Reading

-- | One raw entry of @questions@ or @descriptiveQuestions@.
data Raw = Raw
  { rId :: Text
  , rType :: Text
  , rTitle :: Text
  , rSub :: Maybe Text
  , rReq :: Bool
  , rOrder :: Maybe Double
  , rGroup :: Maybe Text
  , rMulti :: Maybe Bool
  , rInfo :: KM.KeyMap Value
  , rChoices :: [Value]
  , rVal :: Value
  }

data Node = NHeader Group | NQuestion Question

readMSForms :: BL.ByteString -> Conv Form
readMSForms bs = do
  v <- eitherDecode (stripBom bs) `orFail` "Microsoft Forms JSON"
  let (form, qvals, dvals) = case v of
        Array a -> (Null, V.toList a, [])
        _
          | null (keyArr "questions" v), isJust (key "value" v) -> (v, keyArr "value" v, [])
          | otherwise -> (v, keyArr "questions" v, keyArr "descriptiveQuestions" v)
  raws <- mapM toRaw (dvals ++ qvals)
  let sorted = map snd (sortOn (\(i, r) -> (fromMaybe (1 / 0) (rOrder r), i)) (zip [0 :: Int ..] raws))
      matrixIds = [rId r | r <- sorted, isMatrixHeader (rType r)]
      isRow r = rType r == "Question.MatrixChoice" && maybe False (`elem` matrixIds) (rGroup r)
      rows = M.fromListWith (flip (++)) [(g, [r]) | r <- sorted, isRow r, Just g <- [rGroup r]]
      flow = filter (not . isRow) sorted
  nodes <- mapM (readNode rows) flow
  let es = map snd nodes
      pos = M.fromList (zip (map eId es) [0 ..] ++ [(rId r, i) | (i, e) <- zip [0 ..] es, r <- M.findWithDefault [] (eId e) rows])
      (terms, problems) = derive pos es
  mapM_ (uncurry warn) (nub problems)
  meta <- readMeta form
  pure (Form meta (assemble (zip (map fst nodes) (regroup es terms))))

stripBom :: BL.ByteString -> BL.ByteString
stripBom bs = fromMaybe bs (BL.stripPrefix "\xEF\xBB\xBF" bs)

isMatrixHeader :: Text -> Bool
isMatrixHeader = (`elem` ["Question.MatrixChoiceGroup", "Question.Likert", "Question.Matrix"])

toRaw :: Value -> Conv Raw
toRaw v = do
  let i = keyTextOr "" "id" v
  info <- case key "questionInfo" v of
    Just (String s)
      | T.null (T.strip s) -> pure KM.empty
      | otherwise -> case eitherDecode (BL.fromStrict (TE.encodeUtf8 s)) of
          Right (Object o) -> pure o
          _ -> KM.empty <$ warn i "unreadable questionInfo ignored"
    Just (Object o) -> pure o
    _ -> pure KM.empty
  pure
    Raw
      { rId = i
      , rType = keyTextOr "" "type" v
      , rTitle = keyTextOr "" "title" v
      , rSub = nonEmpty =<< keyText "subtitle" v
      , rReq = keyBool "required" v == Just True
      , rOrder = keyNum "order" v
      , rGroup = keyText "groupId" v
      , rMulti = keyBool "allowMultipleValues" v
      , rInfo = info
      , rChoices = sortOn (keyNum "order") (keyArr "choices" v)
      , rVal = v
      }

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

-- | Question-level fields kept as attributes: the rich-text copies of the
-- title and subtitle that the editor stores next to the plain ones.
richFields :: [Text]
richFields = ["formsProRTQuestionTitle", "formsProRTSubtitle"]

rawAttrs :: Raw -> [Text] -> Attrs
rawAttrs r consumed =
  M.fromList $
    [("msforms:" <> K.toText k, x) | (k, x) <- KM.toList (rInfo r), K.toText k `notElem` consumed]
      ++ [("msforms:" <> f, String t) | f <- richFields, Just t <- [keyText f (rVal r)]]

readNode :: M.Map Text [Raw] -> Raw -> Conv (Node, Entry)
readNode rows r
  | rType r == "Question.ColumnGroup" = do
      when (KM.member "BranchInfo" (rInfo r)) $
        warn (rId r) "section jump (after this section go to) kept as an attribute, not converted to show-if"
      let attrs = rawAttrs r [] <> M.fromList [("msforms:subtitle", String t) | Just t <- [rSub r]]
      pure
        ( NHeader (Group (rId r) Page (plain <$> nonEmpty (rTitle r)) Nothing [] attrs)
        , Entry (rId r) True [] []
        )
  | otherwise = do
      (qt, consumed, branches) <- readType rows r
      when (KM.member "BranchInfo" (rInfo r)) $
        warn (rId r) "question-level jump kept as an attribute, not converted to show-if"
      when (isJust (key "image" (rVal r) >>= keyText "resourceUrl")) $ warn (rId r) "question image dropped"
      let labels = case qt of
            QChoice Single _ cs -> [i18nDefault (choiceLabel c) | c <- cs, not (choiceOther c)]
            _ -> []
      pure
        ( NQuestion
            Question
              { qId = rId r
              , qLabel = plain (rTitle r)
              , qHint = plain <$> rSub r
              , qType = qt
              , qRequired = rReq r
              , qRelevant = Nothing
              , qValidation = Nothing
              , qDefault = Nothing
              , qAttrs = rawAttrs r consumed
              }
        , Entry (rId r) False labels branches
        )

infoVal :: Raw -> Value
infoVal = Object . rInfo

readType :: M.Map Text [Raw] -> Raw -> Conv (QType, [Text], [(Text, Target)])
readType rows r = case rType r of
  "Question.Choice" -> do
    (labels, brs) <- case keyArr "Choices" info of
      [] -> pure (entityLabels, [])
      cs -> readInfoChoices (rId r) cs
    let multi = keyNum "ChoiceType" info == Just 2 || rMulti r == Just True
        style = if keyText "OptionDisplayStyle" info == Just "Dropdown" then Dropdown else Radio
        other = [Choice "Other" (plain "Other") True False | keyBool "AllowOtherAnswer" info == Just True]
    when (multi && not (null brs)) $ warn (rId r) "branching on a multi-select question ignored"
    pure
      ( QChoice (if multi then Multiple else Single) style (map (\l -> choice l l) labels ++ other)
      , ["Choices", "ChoiceType", "AllowOtherAnswer", "OptionDisplayStyle"]
      , if multi then [] else brs
      )
  "Question.TextField" -> do
    let rule = key "Validation" info >>= keyNum "rule"
        isNum = keyBool "IsNumber" info == Just True
        qt
          | isNum = QNumber Decimal
          | rule == Just 11 = QText Email
          | keyBool "Multiline" info == Just True = QText LongText
          | otherwise = QText ShortText
    pure
      ( qt
      , ["Multiline", "IsNumber"]
          ++ ["NumberValidationRule" | keyText "NumberValidationRule" info == Just "IsNumber"]
          ++ ["Validation" | maybe True (`elem` [0, 11]) rule]
      , []
      )
  "Question.Rating" -> do
    let lo = maybe 1 round (keyNum "MinRating" info)
        len = maybe 5 round (keyNum "Length" info)
    pure (QScale lo (lo + len - 1) (desc "LeftDescription") (desc "RightDescription"), ["Length", "MinRating", "LeftDescription", "RightDescription"], [])
  t | t `elem` ["Question.NPS", "Question.NetPromoterScore"] ->
    pure (QScale 0 10 (desc "LeftDescription") (desc "RightDescription"), ["LeftDescription", "RightDescription"], [])
  t | t `elem` ["Question.DateTime", "Question.Date"] -> do
    let d = keyBool "Date" info /= Just False
        tm = keyBool "Time" info == Just True
        k
          | d && tm = DateTime
          | tm = Time
          | otherwise = Date
    pure (QDate k, ["Date", "Time"], [])
  "Question.Ranking" -> do
    labels <- labelsAnywhere
    pure (QRanking [choice l l | l <- labels], ["Choices"], [])
  t | isMatrixHeader t -> do
    cols <- labelsAnywhere
    let rs = [choice (rId x) (rTitle x) | x <- M.findWithDefault [] (rId r) rows]
    pure (QMatrix Single rs [choice l l | l <- cols], ["Choices"], [])
  t | t `elem` ["Question.FileUpload", "Question.File"] -> pure (QFile, [], [])
  t -> do
    warn (rId r) ("unsupported type " <> t <> " kept by name")
    pure (QOther t, [], [])
  where
    info = infoVal r
    desc k = plain <$> (nonEmpty =<< keyText k info)
    entityLabels = mapMaybe (keyText "displayText") (rChoices r)
    labelsAnywhere = case entityLabels of
      [] -> fst <$> readInfoChoices (rId r) (keyArr "Choices" info)
      ls -> pure ls

-- | Choices from @questionInfo.Choices@: their text and any jump.
readInfoChoices :: Text -> [Value] -> Conv ([Text], [(Text, Target)])
readInfoChoices qid cs = do
  let label c = fromMaybe "" (keyText "Description" c `orElse` keyText "FormsProDisplayRTText" c)
      known = ["Description", "FormsProDisplayRTText", "IsGenerated", "BranchInfo", "IsMathOption"]
      extras = nub [K.toText k | Object o <- cs, k <- KM.keys o, K.toText k `notElem` known]
      rich = or [maybe False (/= label c) (keyText "FormsProDisplayRTText" c) | c <- cs]
      target b
        | keyBool "ToTheEnd" b == Just True = Just ToEnd
        | otherwise = ToId <$> keyText "TargetQuestionId" b
  unless (null extras) $ warn qid ("choice fields dropped: " <> T.intercalate ", " extras)
  when rich $ warn qid "rich-text choice formatting dropped"
  pure (map label cs, [(label c, t) | c <- cs, Just t <- [key "BranchInfo" c >>= target]])
  where
    orElse a b = maybe b Just a

readMeta :: Value -> Conv Meta
readMeta form = do
  unless (null (keyArr "localeList" form)) $
    warn "msforms" "form translations (localeList) not read: their stored shape is unverified"
  pure
    emptyMeta
      { metaId = keyText "id" form
      , metaTitle = plain <$> (nonEmpty =<< keyText "title" form)
      , metaDescription = plain <$> (nonEmpty =<< keyText "description" form)
      , metaLanguages = maybe [] pure (keyText "defaultLanguage" form)
      , metaAttrs = readDesign "msforms" form <> M.fromList [("msforms:" <> k, x) | k <- ["settings", "thankYouMessage"], Just x <- [key k form]]
      }

-- | Loose questions, then each section header with the entries after it.
assemble :: [(Node, Maybe Expr)] -> [Item]
assemble ns = map item loose ++ sections rest
  where
    isHead (NHeader _, _) = True
    isHead _ = False
    (loose, rest) = break isHead ns
    sections ((NHeader g, rel) : xs) =
      let (mem, more) = break isHead xs
       in ItemGroup g {groupRelevant = rel, groupItems = map item mem} : sections more
    sections (x : xs) = item x : sections xs
    sections [] = []
    item (NQuestion q, rel) = ItemQuestion q {qRelevant = rel}
    item (NHeader g, rel) = ItemGroup g {groupRelevant = rel}

------------------------------------------------------------------------------
-- Writing

-- | What the form holds, flattened: section headers and questions. A
-- question remembers the header it sits under.
data W = WHeader Group | WQ (Maybe Int) Question

writeMSForms :: Form -> Conv BL.ByteString
writeMSForms s@(Form meta items) = do
  unless (null (usedLanguages s)) $ warn "msforms" "translations dropped: written in the default language only"
  case drop 1 (metaLanguages meta) of
    [] -> pure ()
    ls -> warn "msforms" ("extra languages dropped: " <> T.intercalate ", " ls)
  ws <- dedupeIds =<< flattenTop items
  -- Jumps may only target what is actually written.
  branches <- planBranches (filter (not . droppedW) ws)
  (_, descs, qs) <- foldM (emit branches) (1 :: Integer, [], []) ws
  pure . encodePretty $
    obj $
      [ ("id" .=?) . String =<< metaId meta
      , ("title" .=?) . String . i18nDefault =<< metaTitle meta
      , ("description" .=?) . String . i18nDefault =<< metaDescription meta
      , case metaLanguages meta of
          (l : _) -> "defaultLanguage" .=? String l
          [] -> Nothing
      , ("settings" .=?) =<< M.lookup "msforms:settings" (metaAttrs meta)
      , ("thankYouMessage" .=?) =<< M.lookup "msforms:thankYouMessage" (metaAttrs meta)
      , "descriptiveQuestions" .=? toJSON (reverse descs)
      , "questions" .=? toJSON (concat (reverse qs))
      ] ++ map Just (writeDesign "msforms" meta)

-- | Forms ids must be unique: a repeated question or section id gets a
-- numbered suffix. Logic that named it keeps pointing at the first.
dedupeIds :: [W] -> Conv [W]
dedupeIds ws = reverse . snd <$> foldM step (S.fromList (map wId ws), []) (zip [0 :: Int ..] ws)
  where
    firstAt = M.fromListWith (\_ a -> a) [(wId w, i) | (i, w) <- zip [0 ..] ws]
    step (taken, acc) (i, w)
      | M.lookup (wId w) firstAt == Just i = pure (taken, w : acc)
      | otherwise = do
          let fresh = head' [c | n <- [2 :: Int ..], let c = wId w <> "_" <> T.pack (show n), not (S.member c taken)]
          warn (wId w) ("duplicate id renamed to " <> fresh)
          pure (S.insert fresh taken, rename fresh w : acc)
    head' xs = case xs of
      (x : _) -> x
      [] -> ""
    rename n = \case
      WHeader g -> WHeader g {groupId = n}
      WQ h q -> WQ h q {qId = n}

wId :: W -> Text
wId = \case
  WHeader g -> groupId g
  WQ _ q -> qId q

-- | Questions the writer drops (with a warning) rather than writes.
droppedW :: W -> Bool
droppedW = \case
  WQ _ q -> case qType q of
    QHidden -> True
    QCalculate _ -> True
    _ -> False
  WHeader _ -> False

flattenTop :: [Item] -> Conv [W]
flattenTop = go 0 Nothing
  where
    -- @n@ counts headers so far; a question names its page by that number.
    go _ _ [] = pure []
    go n page (i : rest) = case i of
      ItemGroup g | groupKind g == Page -> do
        kids <- concat <$> mapM (inner (Just n)) (groupItems g)
        ((WHeader g : kids) ++) <$> go (n + 1) (Just n) rest
      _ -> do
        here <- inner page i
        unless (null here || page == Nothing) $ warn (itemId i) "item after a page joined that page"
        (here ++) <$> go n page rest
    inner h = \case
      ItemQuestion q -> pure [WQ h q]
      ItemGroup g -> do
        warn (groupId g) "nested group flattened: Microsoft Forms sections do not nest"
        when (isJust (groupRelevant g)) $ warn (groupId g) "show-if on a nested group dropped"
        concat <$> mapM (inner h) (groupItems g)
      ItemNote n _ _ -> [] <$ warn n "display text dropped: Microsoft Forms has no text-only item"
      ItemRaw (Format f) _ -> [] <$ warn "msforms" ("raw " <> f <> " content dropped")

-- | Turn show-ifs into the jumps that express them, warning where the
-- result differs from what was asked.
planBranches :: [W] -> Conv (M.Map Text [(Text, Target)])
planBranches ws = do
  let n = length ws
      idx = [0 .. n - 1]
      wid = \case
        WHeader g -> groupId g
        WQ _ q -> qId q
      headerPos = [j | (j, WHeader _) <- zip idx ws]
      headerAt h = drop h headerPos
      opts = M.fromList [(qId q, [(choiceValue c, i18nDefault (choiceLabel c)) | c <- cs, not (choiceOther c)]) | WQ _ q@Question {qType = QChoice Single _ cs} <- ws]
      pos = M.fromList (zip (map wid ws) idx)
      ownRel = \case
        WHeader g -> groupRelevant g
        WQ _ q -> qRelevant q
      -- choices of each earlier source that must skip this entry
      skipOf j w = case ownRel w of
        Nothing -> M.empty
        Just e -> case allowed opts e of
          Just m | all (\src -> maybe False (< j) (M.lookup src pos)) (M.keys m) ->
            M.mapWithKey (\src ok -> map snd (M.findWithDefault [] src opts) \\ ok) m
          _ -> M.empty
      own = M.fromList [(j, skipOf j w) | (j, w) <- zip idx ws]
      eff j = case ws !! j of
        WQ (Just h) _ | (hp : _) <- headerAt h -> M.unionWith union (M.findWithDefault M.empty j own) (M.findWithDefault M.empty hp own)
        _ -> M.findWithDefault M.empty j own
      skips j src c = c `elem` M.findWithDefault [] src (eff j)
      plan i src labels =
        [ (c, if t == n then ToEnd else ToId (wid (ws !! t)))
        | c <- labels
        , let t = head' n [j | j <- [i + 1 .. n - 1], not (skips j src c)]
        , t > i + 1
        ]
      branches = M.fromList [(src, plan i src (map snd o)) | (i, WQ _ q) <- zip idx ws, let src = qId q, Just o <- [M.lookup src opts]]
      entries = [Entry (wid w) (isHeader w) (maybe [] (map snd) (M.lookup (wid w) opts)) (M.findWithDefault [] (wid w) branches) | w <- ws]
      recon = regroup entries (fst (derive pos entries))
  forM_' (zip ws recon) $ \(w, r) ->
    unless (ownRel w == r) $
      warn (wid w) $
        if isJust r
          then "show-if rewritten as the branching Microsoft Forms can express"
          else "show-if dropped: Microsoft Forms only jumps forward from a single-choice answer"
  pure branches
  where
    isHeader = \case
      WHeader _ -> True
      _ -> False
    head' d = \case
      (x : _) -> x
      [] -> d
    forM_' xs f = mapM_ f xs

-- | The choices (by label) a show-if allows, per source question, when it
-- is a conjunction of equality tests on single-choice questions.
allowed :: M.Map Text [(Text, Text)] -> Expr -> Maybe (M.Map Text [Text])
allowed opts e = M.fromListWith intersect <$> mapM atom (conjuncts e)
  where
    conjuncts = \case
      EBin And a b -> conjuncts a ++ conjuncts b
      x -> [x]
    atom = \case
      EBin Eq (ERef q) (ELit l) -> (\c -> (q, [c])) <$> match q l
      EBin Neq (ERef q) (ELit l) -> (\c -> (q, filter (/= c) (labels q))) <$> match q l
      EBin op a b | op `elem` [Or, And] -> do
        (qa, xa) <- atom a
        (qb, xb) <- atom b
        if qa /= qb then Nothing else Just (qa, if op == Or then filter (\c -> c `elem` xa || c `elem` xb) (labels qa) else xa `intersect` xb)
      ENot x -> pushNot x >>= atom
      _ -> Nothing
    labels q = maybe [] (map snd) (M.lookup q opts)
    match q l = do
      os <- M.lookup q opts
      let t = litText l
      case [lab | (v, lab) <- os, v == t || lab == t] of
        (lab : _) -> Just lab
        [] -> Nothing
    litText = \case
      LStr t -> t
      LNum x -> if x == fromIntegral (round x :: Integer) then T.pack (show (round x :: Integer)) else T.pack (show x)
      LBool b -> if b then "true" else "false"

type Acc = (Integer, [Value], [[Value]])

emit :: M.Map Text [(Text, Target)] -> Acc -> W -> Conv Acc
emit branches (k, descs, qs) = \case
  WHeader g -> do
    let (top, info) = splitAttrs (groupAttrs g)
    pure
      ( k + 1
      , obj
          ( [ "id" .=? String (groupId g)
            , "type" .=? "Question.ColumnGroup"
            , "title" .=? String (maybe "" i18nDefault (groupTitle g))
            , "order" .=? orderAt k
            , "questionInfo" .=? infoJSON info
            ]
              ++ map Just top
          )
          : descs
      , qs
      )
  WQ _ q -> do
    out <- writeQuestion q (M.findWithDefault [] (qId q) branches)
    case out of
      Nothing -> pure (k, descs, qs)
      Just (fields, rows) -> do
        let (top, attrInfo) = splitAttrs (qAttrs q)
            info = KM.union (oInfo fields) attrInfo
            header =
              obj $
                [ "id" .=? String (qId q)
                , "type" .=? String (oType fields)
                , "title" .=? String (i18nDefault (qLabel q))
                , ("subtitle" .=?) . String . i18nDefault =<< qHint q
                , "required" .=? Bool (qRequired q)
                , "order" .=? orderAt k
                , if oMulti fields then "allowMultipleValues" .=? Bool True else Nothing
                , "questionInfo" .=? infoJSON info
                , if null (oChoices fields) then Nothing else "choices" .=? toJSON (oChoices fields)
                ]
                  ++ map Just top
            rowVals =
              [ obj ["id" .=? String (choiceValue r), "type" .=? "Question.MatrixChoice", "title" .=? String (i18nDefault (choiceLabel r)), "groupId" .=? String (qId q), "order" .=? orderAt (k + n)]
              | (n, r) <- zip [1 ..] rows
              ]
        pure (k + 1 + fromIntegral (length rows), descs, (header : rowVals) : qs)

-- | Orders as the editor spaces them: a million apart, offset by 500.
orderAt :: Integer -> Value
orderAt k = toJSON (k * 1000000 + 500)

infoJSON :: KM.KeyMap Value -> Value
infoJSON info
  | KM.null info = Null
  | otherwise = String (TE.decodeUtf8 (BL.toStrict (encode (Object info))))

-- | This format's attributes: top-level fields, and @questionInfo@ keys.
splitAttrs :: Attrs -> ([(K.Key, Value)], KM.KeyMap Value)
splitAttrs attrs =
  ( [(K.fromText (top k), v) | (k, v) <- mine, top k `elem` "subtitle" : richFields]
  , KM.fromList [(K.fromText (top k), v) | (k, v) <- mine, top k `notElem` "subtitle" : richFields]
  )
  where
    mine = [(k, v) | (k, v) <- M.toList attrs, "msforms:" `T.isPrefixOf` k]
    top = T.drop (T.length "msforms:")

data Out = Out
  { oType :: Text
  , oInfo :: KM.KeyMap Value
  , oChoices :: [Value]
  , oMulti :: Bool
  }

out :: Text -> [(K.Key, Value)] -> Out
out t info = Out t (KM.fromList info) [] False

writeQuestion :: Question -> [(Text, Target)] -> Conv (Maybe (Out, [Choice]))
writeQuestion q brs = do
  when (isJust (qValidation q)) $ warn (qId q) "validation dropped: its stored shape is unverified"
  when (isJust (qDefault q)) $ warn (qId q) "default value dropped"
  case qType q of
    QChoice sel style cs -> do
      let others = filter choiceOther cs
          regular = filter (not . choiceOther) cs
      codes regular
      warnFixedCodes (qId q) (Just "Other") Nothing others
      when (any ((/= plain "Other") . choiceLabel) others) $ warn (qId q) "\"other\" label replaced by Other"
      when (length others > 1) $ warn (qId q) "several \"other\" choices merged into one"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive (none of the above) flag dropped"
      when (style == Buttons) $ warn (qId q) "button choices written as a list"
      let label c = i18nDefault (choiceLabel c)
          choiceV c = object (("Description" .= label c) : [("BranchInfo", targetJSON t) | Just t <- [lookup (label c) brs]])
      pure . Just . (,[]) $
        (out "Question.Choice"
          [ ("Choices", toJSON (map choiceV regular))
          , ("ChoiceType", toJSON (if sel == Multiple then 2 else 1 :: Int))
          , ("AllowOtherAnswer", Bool (not (null others)))
          , ("OptionDisplayStyle", if style == Dropdown then "Dropdown" else "ListAll")
          ])
          {oMulti = sel == Multiple}
    QText k -> do
      when (k `elem` [Url, Phone]) $ warn (qId q) "url/phone check dropped: written as short text"
      pure . Just . (,[]) . out "Question.TextField" $
        ("Multiline", Bool (k == LongText)) : [("Validation", object ["rule" .= (11 :: Int)]) | k == Email]
    QNumber k -> do
      when (k == Integer) $ warn (qId q) "whole-number restriction dropped: written as a number"
      pure . Just . (,[]) $ out "Question.TextField" [("Multiline", Bool False), ("IsNumber", Bool True), ("NumberValidationRule", "IsNumber")]
    QDate k -> do
      unless (k == Date) $ warn (qId q) "time part written as the Time flag, which is unverified"
      pure . Just . (,[]) $ out "Question.DateTime" [("Date", Bool (k /= Time)), ("Time", Bool (k /= Date))]
    QScale 0 10 l h -> pure . Just . (,[]) $ out "Question.NPS" (labels l h)
    QScale lo hi l h -> do
      let len = hi - lo + 1
      unless (lo == 1 && len >= 2 && len <= 10) $ warn (qId q) "rating range outside the editor's 1 to 2-10"
      pure . Just . (,[]) . out "Question.Rating" $
        [("Length", toJSON len), ("MinRating", toJSON lo)]
          ++ [("RatingShape", "Number") | not (M.member "msforms:RatingShape" (qAttrs q))]
          ++ labels l h
    QRanking cs -> do
      codes cs
      pure . Just . (,[]) $ (out "Question.Ranking" []) {oChoices = entities cs, oMulti = True}
    QMatrix sel rows cols -> do
      when (sel == Multiple) $ warn (qId q) "multi-select matrix written as a single-select Likert"
      codes cols
      pure (Just ((out "Question.MatrixChoiceGroup" []) {oChoices = entities cols}, rows))
    QBoolean -> do
      warn (qId q) "yes/no written as a Yes/No choice"
      -- Written exactly as the Yes/No choice it reads back as.
      writeQuestion q {qType = QChoice Single Radio [choice "Yes" "Yes", choice "No" "No"], qValidation = Nothing, qDefault = Nothing} []
    QFile -> pure . Just . (,[]) $ out "Question.FileUpload" []
    QGeo -> do
      warn (qId q) "geopoint written as a text box"
      pure . Just . (,[]) $ out "Question.TextField" [("Multiline", Bool False)]
    QHidden -> Nothing <$ warn (qId q) "hidden field dropped: Microsoft Forms has none"
    QCalculate _ -> Nothing <$ warn (qId q) "calculation dropped: Microsoft Forms has none"
    QOther t -> do
      warn (qId q) ("unsupported type " <> t <> " written by name")
      pure . Just . (,[]) $ out t []
  where
    codes cs =
      when (any (\c -> choiceValue c /= i18nDefault (choiceLabel c)) cs) $
        warn (qId q) "choice codes replaced by their labels (Microsoft Forms stores choice text only)"
    labels l h = [("LeftDescription", String (i18nDefault t)) | Just t <- [l]] ++ [("RightDescription", String (i18nDefault t)) | Just t <- [h]]
    entities cs =
      [ object ["displayText" .= i18nDefault (choiceLabel c), "key" .= i18nDefault (choiceLabel c), "order" .= n]
      | (n, c) <- zip [1 :: Int ..] cs
      ]

targetJSON :: Target -> Value
targetJSON = \case
  ToEnd -> object ["ToTheEnd" .= True]
  ToId t -> object ["TargetQuestionId" .= t]
