-- | IMS QTI 2.1 (1EdTech): an @assessmentTest@ of @testPart@ /
-- @assessmentSection@s whose @assessmentItemRef@s carry show-if
-- @preCondition@s (QTI expression XML) and point at @assessmentItem@s
-- holding an interaction (choice, text entry, extended text, slider,
-- order, match, upload). The writer emits one file with each item
-- embedded in its @assessmentItemRef@ rather than an IMS content package
-- zip; the reader takes that, a bare @assessmentItem@, or a test whose
-- items are embedded. @branchRule@ jumps become preconditions.
module Panform.Format.QTI (readQTI, writeQTI) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.List (nub, sortOn)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Expr (pushNot)
import Panform.Format.Codebook (fmtNum, readNum, slug, tshow)
import Panform.XmlKit

interactions :: [Text]
interactions =
  [ "choiceInteraction"
  , "inlineChoiceInteraction"
  , "textEntryInteraction"
  , "extendedTextInteraction"
  , "sliderInteraction"
  , "orderInteraction"
  , "matchInteraction"
  , "uploadInteraction"
  ]

------------------------------------------------------------------------------
-- Reading

-- | A response variable (@item.RESPONSE@): the question it answers and its
-- choice identifiers mapped back to codes.
data Var = Var {vQuestion :: Text, vCodes :: M.Map Text Text}

type Vars = M.Map Text Var

-- | A condition still in QTI XML: a @preCondition@ (its expression is the
-- first child), a bare expression, or what branch rules add to them.
data Cond = Pre Element | Raw Element | CNot Cond | CAll [Cond]

-- | The most condition terms the branch rules of one test may expand to.
-- A jump over @n@ items copies its condition onto each of them, so a
-- small file can ask for a great deal of logic.
maxBranchTerms :: Int
maxBranchTerms = 1000000

-- | Items and sections before their conditions are resolved, which needs
-- every response variable in the test.
data Pending = PItem Item [Cond] [Element] | PGroup Group [Cond] [Pending]

readQTI :: BL.ByteString -> Conv Form
readQTI bs = do
  root <- parseXml "QTI XML" bs
  case localName root of
    "assessmentItem" -> do
      (items, _) <- readItem root
      pure (Form emptyMeta {metaTitle = plain <$> nonBlank (attr "title" root)} items)
    "assessmentTest" -> do
      let step (acc, budget) sec = (\(xs, left) -> (xs : acc, left)) <$> readSection True budget sec
      sections <- concat . reverse . fst <$> foldM step ([], maxBranchTerms) (concatMap (children "assessmentSection") (children "testPart" root))
      let vars = M.unions (map snd sections)
      items <- mapM (resolve vars . fst) sections
      let title = nonBlank (attr "title" root)
          -- the writer names an unidentified test after its title
          ident = nonBlank (attr "identifier" root) >>= \i -> if i == slug (fromMaybe "Survey" title) then Nothing else Just i
      pure (Form emptyMeta {metaId = ident, metaTitle = plain <$> title} items)
    other -> failWith ("QTI: unsupported root element " <> other)

-- | A section's items, given how many branch condition terms may still
-- be generated; returns what is left of that allowance.
readSection :: Bool -> Int -> Element -> Conv ([(Pending, Vars)], Int)
readSection top budget0 s = do
  let sub (acc, budget) c
        | localName c == "assessmentSection" = (\(xs, left) -> (xs : acc, left)) <$> readSection False budget c
        | otherwise = (\xs -> (xs : acc, budget)) <$> readRef c
  (found, budget1) <- foldM sub ([], budget0) (childrenAny ["assessmentItemRef", "assessmentSection"] s)
  let parts = concat (reverse found)
      sid = attr "identifier" s
      title = nonBlank (attr "title" s) >>= \t -> if t == sid then Nothing else Just t
      g = Group sid (if top then Page else Section) (plain <$> title) Nothing [] M.empty
  unless (null (children "branchRule" s)) $ warn sid "section branch rule dropped"
  (routed, budget2) <- branches budget1 (map fst parts)
  -- a top-level invisible section holds items that belong to no page
  if top && attr "visible" s == "false" && null (children "preCondition" s)
    then pure (zip routed (map snd parts), budget2)
    else pure ([(PGroup g (map Pre (children "preCondition" s)) routed, M.unions (map snd parts))], budget2)

readRef :: Element -> Conv [(Pending, Vars)]
readRef r = case child "assessmentItem" r of
  Nothing -> [] <$ warn (attr "identifier" r) ("external item " <> attr "href" r <> " not resolved (content packages are not read)")
  Just it -> do
    (items, vars) <- readItem it
    let required = maybe False ((== "false") . attr "allowSkipping") (child "itemSessionControl" r)
        mark = \case
          ItemQuestion q -> ItemQuestion q {qRequired = qRequired q || required}
          i -> i
        lastIx = length items - 1
    -- branch rules act after the item's last question
    pure [(PItem (mark i) (map Pre (children "preCondition" r)) (if n == lastIx then children "branchRule" r else []), vars) | (n, i) <- zip [0 ..] items]

-- | A forward jump: the item it leaves, what must hold for its rule to
-- fire (the rule's condition, and no earlier rule of the item firing), the
-- index it lands on, and how many XML elements the conditions hold.
data Jump = Jump {jSrc :: Int, jFire :: [Cond], jEnd :: Int, jCost :: Int}

-- | A branch rule on item X that jumps to T when C holds means the items
-- between X and T are not shown when the rule is taken; @EXIT_SECTION@
-- skips the rest of the section. The rule is taken only when X itself was
-- shown (its own preconditions held and no earlier jump passed over it)
-- and no earlier rule of X fired, so each skipped item gets "not (X shown
-- and C)". Backward jumps, and targets outside the section, cannot become
-- show-if conditions and are dropped with a warning.
branches :: Int -> [Pending] -> Conv ([Pending], Int)
branches budget0 ps = do
  forM_ (zip [0 :: Int ..] ps) $ \case
    (k, PItem i _ bs) -> forM_ bs $ \b -> case (condOf b, target b) of
      (_, Nothing) -> warn (itemId i) ("branch to " <> attr "target" b <> " dropped")
      (Nothing, _) -> warn (itemId i) ("branch to " <> attr "target" b <> " dropped, it has no condition")
      (_, Just end) | end <= k -> warn (itemId i) ("backward branch to " <> attr "target" b <> " dropped")
      _ -> pure ()
    _ -> pure ()
  (out, left, _, _) <- foldM step ([], budget0, [], M.empty) (zip [0 :: Int ..] ps)
  pure (reverse out, left)
  where
    n = length ps
    index = M.fromListWith (\_ first -> first) (zip (map pendingId ps) [0 :: Int ..])
    target b = if attr "target" b == "EXIT_SECTION" then Just n else M.lookup (attr "target" b) index
    condOf b = listToMaybe (childrenAny exprElements b)
    size e = 1 + length (allDescendants e)
    own = M.fromList (zip [0 :: Int ..] (map pendingPre ps))
    ownPre k = M.findWithDefault [] k own
    jumpsFrom =
      M.fromList
        [ ( k
          , [ Jump k (map (CNot . Raw) earlier ++ [Raw c]) end (sum (map size (c : earlier)))
            | (m, b) <- zip [0 :: Int ..] bs
            , Just c <- [condOf b]
            , Just end <- [target b]
            , end > k + 1
            , let earlier = [c' | b' <- take m bs, Just c' <- [condOf b']]
            ]
          )
        | (k, PItem _ _ bs) <- zip [0 :: Int ..] ps
        , not (null bs)
        ]
    -- Walks the items in order. @active@ holds the jumps that pass over
    -- the current item; @covers@ remembers them per item, sorted by where
    -- they land, for jumps that leave that item.
    step (acc, budget, active, covers) (i, p) = do
      let active' = [j | j <- active ++ M.findWithDefault [] (i - 1) jumpsFrom, jEnd j > i]
          term (ts, b) j = case taken covers i j b of
            Just (c, b') -> pure (CNot c : ts, b')
            Nothing -> failWith ("QTI: branch rules expand to more than " <> tshow maxBranchTerms <> " condition terms")
      (terms, budget') <- foldM term ([], budget) active'
      pure (addConds (reverse terms) p : acc, budget', active', M.insert i (sortOn jEnd active') covers)
    -- "This jump was taken", for an item at index @i@ that no jump landing
    -- after @i@ passed over (each of those has its own term on the item):
    -- its source's preconditions held, no jump that landed at or before
    -- @i@ passed over the source, and its rule fired.
    taken covers i j budget
      | cost > budget = Nothing
      | otherwise = do
          let inner = takeWhile ((<= i) . jEnd) (M.findWithDefault [] (jSrc j) covers)
              add (cs, b) m = (\(c, b') -> (CNot c : cs, b')) <$> taken covers i m b
          (nots, left) <- foldM add ([], budget - cost) inner
          pure (CAll (ownPre (jSrc j) ++ reverse nots ++ jFire j), left)
      where
        cost = jCost j + sum [size x | Pre x <- ownPre (jSrc j)]
    addConds cs = \case
      PItem i pre bs -> PItem i (pre ++ cs) bs
      PGroup g pre kids -> PGroup g (pre ++ cs) kids
    pendingPre = \case
      PItem _ pre _ -> pre
      PGroup _ pre _ -> pre
    pendingId = \case
      PItem i _ _ -> itemId i
      PGroup g _ _ -> groupId g

resolve :: Vars -> Pending -> Conv Item
resolve vars = \case
  PGroup g conds kids -> do
    rel <- conditions vars (groupId g) conds
    items <- mapM (resolve vars) kids
    pure (ItemGroup g {groupRelevant = rel, groupItems = items})
  PItem i conds _ -> do
    rel <- conditions vars (itemId i) conds
    pure $ case i of
      ItemQuestion q -> ItemQuestion q {qRelevant = rel}
      ItemNote n t _ -> ItemNote n t rel
      _ -> i

conditions :: Vars -> Text -> [Cond] -> Conv (Maybe Expr)
conditions vars at conds = do
  es <- mapM go conds
  pure (conj (concat [e | Just e <- es]))
  where
    conj = \case
      [] -> Nothing
      (x : xs) -> Just (foldl (EBin And) x xs)
    -- 'Nothing' when a part cannot be read: the whole condition it belongs
    -- to is then dropped (with the warning), never half of it
    go :: Cond -> Conv (Maybe [Expr])
    go = \case
      Pre x -> maybe (pure (Just [])) expr (listToMaybe (childrenAny exprElements x))
      Raw x -> expr x
      CNot c -> fmap (\es -> [maybe (ELit (LBool False)) (\x -> fromMaybe (ENot x) (pushNot x)) (conj es)]) <$> go c
      -- a jump that is always taken adds nothing to "its item was shown"
      CAll cs -> fmap (filter (/= ELit (LBool True)) . concat) . sequence <$> mapM go cs
    expr x = case fromQti vars x of
      Right e -> pure (Just [e])
      Left what -> Nothing <$ warn at ("condition dropped, cannot read QTI " <> what)

exprElements :: [Text]
exprElements = ["and", "or", "not", "match", "equal", "lt", "lte", "gt", "gte", "member", "isNull", "variable", "baseValue", "sum", "subtract", "product", "divide", "null"]

-- | Negation as the writer spells it: @not(match)@ is @!=@, @not(isNull)@
-- is "answered"; anything else stays a 'ENot'.
neg :: Expr -> Expr
neg = \case
  EBin Eq a b -> EBin Neq a b
  ENot x -> x
  x -> ENot x

fromQti :: Vars -> Element -> Either Text Expr
fromQti vars e = case localName e of
  "and" -> nary And
  "or" -> nary Or
  "sum" -> nary Add
  "product" -> nary Mul
  "subtract" -> binary Sub
  "divide" -> binary Div
  "not" -> neg <$> one
  "match" -> cmp Eq
  "equal" -> cmp Eq
  "lt" -> cmp Lt
  "lte" -> cmp Le
  "gt" -> cmp Gt
  "gte" -> cmp Ge
  "member" -> case kids of
    [b, v] | localName b == "baseValue", localName v == "variable" -> let (q, codes) = var v in Right (ESelected q (M.findWithDefault (textOf b) (textOf b) codes))
    _ -> Left "member"
  "isNull" -> case kids of
    [v] | localName v == "variable" -> Right (ENot (EAnswered (fst (var v))))
    _ -> Left "isNull"
  "variable" -> Right (ERef (fst (var e)))
  "baseValue" -> Right (lit M.empty e)
  other -> Left other
  where
    kids = childrenAny exprElements e
    one = case kids of
      [x] -> fromQti vars x
      _ -> Left (localName e)
    nary op = case mapM (fromQti vars) kids of
      Right (x : xs) -> Right (foldl (EBin op) x xs)
      Right [] -> Left (localName e)
      Left w -> Left w
    binary op = case kids of
      [a, b] -> EBin op <$> fromQti vars a <*> fromQti vars b
      _ -> Left (localName e)
    cmp op = case kids of
      [v, b] | localName v == "variable", localName b == "baseValue" -> let (q, codes) = var v in Right (EBin op (ERef q) (lit codes b))
      [b, v] | localName v == "variable", localName b == "baseValue" -> let (q, codes) = var v in Right (EBin (flipOp op) (ERef q) (lit codes b))
      [a, b] -> EBin op <$> fromQti vars a <*> fromQti vars b
      _ -> Left (localName e)
    var v =
      let ident = attr "identifier" v
       in maybe (fromMaybe ident (T.stripSuffix ".RESPONSE" ident), M.empty) (\x -> (vQuestion x, vCodes x)) (M.lookup ident vars)
    lit codes b = case attr "baseType" b of
      t | t `elem` ["integer", "float"], Just n <- readNum (textOf b) -> ELit (LNum n)
      "boolean" -> ELit (LBool (textOf b == "true"))
      _ -> ELit (LStr (M.findWithDefault (textOf b) (textOf b) codes))
    flipOp = \case
      Lt -> Gt
      Gt -> Lt
      Le -> Ge
      Ge -> Le
      o -> o

-- | The questions (or a note) in an item, and the response variables
-- they define, keyed @item.RESPONSE@.
readItem :: Element -> Conv ([Item], Vars)
readItem it = do
  let iid = attr "identifier" it
      decls = M.fromList [(attr "identifier" d, d) | d <- children "responseDeclaration" it]
      body = child "itemBody" it
      ordered = maybe [] (filter ((`elem` interactions) . localName) . allDescendants) body
      -- each text-only block exactly (what the writer emits), others flattened
      bodyText = T.intercalate "\n" [t | b <- maybe [] (childrenAny blockNames) body, let t = blockText b, not (T.null t)]
      blockText b
        | any ((`elem` interactions) . localName) (b : allDescendants b) = deepTextExcept interactions b
        | otherwise = exactText b
      blockNames = ["p", "div", "blockquote", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "table", "pre"]
      hint = plain <$> (body >>= child "rubricBlock" >>= \r -> nonBlank (maybe (deepText r) exactText (child "p" r)))
      scored = any (not . null) [children "outcomeDeclaration" it, children "responseProcessing" it, concatMap (children "correctResponse") (M.elems decls)]
  when scored $ warn iid "scoring and response processing dropped"
  forM_ (nub [localName e | e <- maybe [] allDescendants body, "Interaction" `T.isSuffixOf` localName e, localName e `notElem` interactions]) $ \n ->
    warn iid ("unsupported " <> n <> " dropped")
  when (any ((`elem` ["img", "object", "audio", "video"]) . localName) (maybe [] allDescendants body)) $
    warn iid "images and media dropped"
  -- body text beside a prompted interaction is a stimulus, kept as a note
  let prompted = any (isJust . child "prompt") ordered
      stimulus = [ItemNote (iid <> "_stimulus") (plain bodyText) Nothing | prompted, not (T.null bodyText)]
  case ordered of
    [] -> pure ([ItemNote iid (plain bodyText) Nothing], M.empty)
    [x] -> do
      (q, v) <- readInteraction iid bodyText hint decls x
      pure (stimulus ++ [ItemQuestion q], M.singleton (iid <> "." <> attr "responseIdentifier" x) v)
    xs -> do
      qs <- forM xs $ \x -> do
        let rid = attr "responseIdentifier" x
        (q, v) <- readInteraction (iid <> "_" <> rid) bodyText hint decls x
        pure (ItemQuestion q, (iid <> "." <> rid, v))
      pure (stimulus ++ map fst qs, M.fromList (map snd qs))

readInteraction :: Text -> Text -> Maybe I18n -> M.Map Text Element -> Element -> Conv (Question, Var)
readInteraction qid bodyText hint decls x = do
  let kind = localName x
      decl = M.lookup (attr "responseIdentifier" x) decls
      baseType = maybe "" (attr "baseType") decl
      cls = T.words (attr "class" x)
      prompt = exactText <$> child "prompt" x
      label = plain (fromMaybe bodyText prompt)
      choicesOf n = [(attr "identifier" c, Choice (fromMaybe (attr "identifier" c) (attrMaybe "label" c)) (plain (exactText c)) False False) | c <- descendants n x]
      codesOf cs = M.fromList [(i, choiceValue c) | (i, c) <- cs]
      style
        | "dropdown" `elem` cls = Dropdown
        | "buttons" `elem` cls = Buttons
        | otherwise = Radio
      num a d = fromMaybe d (readNum (attr a x))
  (qt, codes) <- case kind of
    "choiceInteraction"
      | "boolean" `elem` cls -> pure (QBoolean, codesOf (choicesOf "simpleChoice"))
      | otherwise ->
          let cs = choicesOf "simpleChoice"
           in pure (QChoice (if num "maxChoices" 1 == 1 then Single else Multiple) style (map snd cs), codesOf cs)
    "inlineChoiceInteraction" -> let cs = choicesOf "inlineChoice" in pure (QChoice Single Dropdown (map snd cs), codesOf cs)
    "textEntryInteraction" -> pure . (,M.empty) $ case baseType of
      "integer" -> QNumber Integer
      "float" -> QNumber Decimal
      _
        | "email" `elem` cls -> QText Email
        | "url" `elem` cls -> QText Url
        | "phone" `elem` cls -> QText Phone
        | "date" `elem` cls -> QDate Date
        | "time" `elem` cls -> QDate Time
        | "datetime" `elem` cls -> QDate DateTime
        | "geopoint" `elem` cls -> QGeo
        | otherwise -> QText ShortText
    "extendedTextInteraction" -> pure (QText LongText, M.empty)
    "sliderInteraction" -> do
      when (baseType == "float" || num "step" 1 /= 1) $ warn qid "non-integer slider steps rounded"
      pure (QScale (round (num "lowerBound" 0)) (round (num "upperBound" 100)) Nothing Nothing, M.empty)
    "orderInteraction" -> let cs = choicesOf "simpleChoice" in pure (QRanking (map snd cs), codesOf cs)
    "matchInteraction" -> do
      unless ("matrix" `elem` cls) $ warn qid "match interaction read as a matrix"
      case children "simpleMatchSet" x of
        [rs, cs] -> do
          let opts s = [(attr "identifier" c, Choice (fromMaybe (attr "identifier" c) (attrMaybe "label" c)) (plain (exactText c)) False False) | c <- children "simpleAssociableChoice" s]
              rows = opts rs
              cols = opts cs
              single = all (\c -> attr "matchMax" c == "1") (children "simpleAssociableChoice" rs)
          pure (QMatrix (if single then Single else Multiple) (map snd rows) (map snd cols), codesOf (rows ++ cols))
        _ -> (QOther "matchInteraction", M.empty) <$ warn qid "match interaction without two sets kept by name"
    "uploadInteraction" -> pure (QFile, M.empty)
    other -> (QOther other, M.empty) <$ warn qid ("unsupported interaction " <> other)
  let defaults = [M.findWithDefault (textOf v) (textOf v) codes | Just d <- [decl], dv <- children "defaultValue" d, v <- children "value" dv]
      minimum' n = fromMaybe 0 (readNum (attr n x))
  pure
    ( (question qid "" qt)
        { qLabel = label
        , qHint = hint
        , qDefault = listToMaybe defaults
        , qRequired = minimum' "minChoices" >= 1 || minimum' "minStrings" >= 1
        }
    , Var qid codes
    )

------------------------------------------------------------------------------
-- Writing

-- | How logic refers to a question's answer.
data Kind = KChoice Selection (M.Map Text Text) | KBool | KNum | KText

writeQTI :: Form -> Conv BL.ByteString
writeQTI (Form meta items) = do
  -- only questions that are written; the first of a duplicated id wins, as
  -- it does when the test is read back
  let kinds = M.fromListWith (\_ old -> old) [(qId q, kindOf q) | q <- questions (Form meta items), written (qType q)]
      written = \case
        QHidden -> False
        QCalculate _ -> False
        _ -> True
      title = maybe "Survey" i18nDefault (metaTitle meta)
  when (isJust (metaDescription meta)) $ warn "qti" "survey description dropped"
  when (maybe False (not . null . i18nLangs) (metaTitle meta)) $ warn "qti" "survey title translations dropped"
  sections <- topSections kinds items
  pure . renderXml $
    el
      "assessmentTest"
      [ ("xmlns", "http://www.imsglobal.org/xsd/imsqti_v2p1")
      , ("identifier", fromMaybe (slug title) (metaId meta))
      , ("title", title)
      ]
      [el "testPart" [("identifier", "part1"), ("navigationMode", "linear"), ("submissionMode", "individual")] sections]

kindOf :: Question -> Kind
kindOf q = case qType q of
  QChoice s _ cs -> KChoice s (idents cs)
  QRanking cs -> KChoice Multiple (idents cs)
  QMatrix _ rs cs -> KChoice Multiple (idents (rs ++ cs))
  QBoolean -> KBool
  QNumber _ -> KNum
  QScale {} -> KNum
  _ -> KText

-- | Choice codes as QTI identifiers: a code that is not a valid
-- identifier becomes @c1@, @c2@, ... by position (the code itself is kept
-- in the choice's @label@).
idents :: [Choice] -> M.Map Text Text
idents cs = M.fromList [(choiceValue c, if validIdent (choiceValue c) then choiceValue c else "c" <> tshow n) | (n, c) <- zip [1 :: Int ..] cs]

validIdent :: Text -> Bool
validIdent t = case T.uncons t of
  Just (c, rest) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x `elem` ("_-." :: String)) rest
  Nothing -> False

-- | Top-level pages become sections; loose items are gathered into one.
topSections :: M.Map Text Kind -> [Item] -> Conv [Node]
topSections kinds = go (1 :: Int)
  where
    go _ [] = pure []
    go n (ItemGroup g : rest) | groupKind g == Page = (:) <$> section True kinds g <*> go n rest
    go n is = do
      let (loose, rest) = break isPage is
          sid = "section" <> tshow n
      -- an invisible section: items that were on no page
      s <- section False kinds (Group sid Page Nothing Nothing loose M.empty)
      -- one with nothing written in it would vanish on reading
      let keep = case s of
            El _ _ kids -> not (null kids)
            _ -> True
      ([s | keep] ++) <$> go (if keep then n + 1 else n) rest
    isPage = \case
      ItemGroup g -> groupKind g == Page
      _ -> False

section :: Bool -> M.Map Text Kind -> Group -> Conv Node
section visible kinds g = do
  when (groupKind g == Repeat) $ warn (groupId g) "repeat written as a plain section"
  when (maybe False (not . null . i18nLangs) (groupTitle g)) $ warn (groupId g) "section title translations dropped"
  pre <- precondition kinds (groupId g) (groupRelevant g)
  parts <- catMaybes' <$> mapM (part kinds) (groupItems g)
  pure $
    el
      "assessmentSection"
      [("identifier", groupId g), ("title", fromMaybe (groupId g) (groupTitle g >>= nonBlank . i18nDefault)), ("visible", if visible then "true" else "false")]
      (pre ++ parts)
  where
    catMaybes' = concat

part :: M.Map Text Kind -> Item -> Conv [Node]
part kinds = \case
  ItemGroup g -> do
    when (groupKind g == Page) $ warn (groupId g) "nested page written as a section"
    pure <$> section True kinds g
  ItemRaw (Format f) _ -> [] <$ warn "qti" ("raw " <> f <> " content dropped")
  ItemNote i t rel -> do
    pre <- precondition kinds i rel
    pure [itemRef i pre False (el "assessmentItem" (itemAttrs i) [el "itemBody" [] [leaf "p" [] (i18nDefault t)]])]
  ItemQuestion q -> do
    pre <- precondition kinds (qId q) (qRelevant q)
    body <- itemFor q
    pure [itemRef (qId q) pre (qRequired q) b | Just b <- [body]]

itemRef :: Text -> [Node] -> Bool -> Node -> Node
itemRef i pre required item =
  el "assessmentItemRef" [("identifier", i), ("href", i <> ".xml")] $
    pre ++ [el "itemSessionControl" [("allowSkipping", "false"), ("validateResponses", "true")] [] | required] ++ [item]

itemAttrs :: Text -> [(Text, Text)]
itemAttrs i = [("identifier", i), ("title", i), ("adaptive", "false"), ("timeDependent", "false")]

itemFor :: Question -> Conv (Maybe Node)
itemFor q = do
  let i = qId q
      hasTranslations = not (null (i18nLangs (qLabel q))) || case qType q of
        QChoice _ _ cs -> any (not . null . i18nLangs . choiceLabel) cs
        _ -> False
  unless (validIdent i) $ warn i "not a valid QTI identifier"
  when hasTranslations $ warn i "translations dropped (a QTI item has one language)"
  when (isJust (qValidation q)) $ warn i "validation dropped"
  let label = i18nDefault (qLabel q)
      rubric = [el "rubricBlock" [("view", "candidate")] [leaf "p" [] (i18nDefault h)] | Just h <- [qHint q]]
      prompt = [leaf "prompt" [] label | not (T.null label)]
      decl card base dflt =
        el "responseDeclaration" [("identifier", "RESPONSE"), ("cardinality", card), ("baseType", base)] [el "defaultValue" [] [leaf "value" [] d] | Just d <- [dflt]]
      item card base dflt body = pure (Just (el "assessmentItem" (itemAttrs i) [decl card base dflt, el "itemBody" [] (rubric ++ body)]))
      entry cls base = item "single" base (qDefault q) [leaf "p" [] label, el "p" [] [el "textEntryInteraction" ([("responseIdentifier", "RESPONSE")] ++ [("class", c) | Just c <- [cls]]) []]]
      simpleChoices cs =
        let ids = idents cs
         in [ el "simpleChoice" ([("identifier", ident)] ++ [("label", choiceValue c) | ident /= choiceValue c]) [Tx (i18nDefault (choiceLabel c))]
            | c <- cs
            , let ident = M.findWithDefault (choiceValue c) (choiceValue c) ids
            ]
      dfltIdent cs = qDefault q >>= \d -> M.lookup d (idents cs)
  case qType q of
    QChoice sel style cs -> do
      when (any choiceOther cs) $ warn i "\"other, specify\" text box dropped"
      when (any choiceExclusive cs) $ warn i "exclusive (none of the above) flag dropped"
      let cls = case style of
            Dropdown -> [("class", "dropdown")]
            Buttons -> [("class", "buttons")]
            Radio -> []
      item (if sel == Single then "single" else "multiple") "identifier" (dfltIdent cs) $
        [el "choiceInteraction" ([("responseIdentifier", "RESPONSE"), ("shuffle", "false"), ("maxChoices", if sel == Single then "1" else "0")] ++ cls) (prompt ++ simpleChoices cs)]
    QBoolean ->
      item "single" "identifier" (qDefault q) $
        [el "choiceInteraction" [("responseIdentifier", "RESPONSE"), ("shuffle", "false"), ("maxChoices", "1"), ("class", "boolean")] (prompt ++ simpleChoices [choice "true" "Yes", choice "false" "No"])]
    QRanking cs -> item "ordered" "identifier" Nothing [el "orderInteraction" [("responseIdentifier", "RESPONSE"), ("shuffle", "false")] (prompt ++ simpleChoices cs)]
    QMatrix sel rows cols -> do
      let set cs matchMax =
            let ids = idents (rows ++ cols)
             in el "simpleMatchSet" [] [el "simpleAssociableChoice" ([("identifier", ident), ("matchMax", matchMax)] ++ [("label", choiceValue c) | ident /= choiceValue c]) [Tx (i18nDefault (choiceLabel c))] | c <- cs, let ident = M.findWithDefault (choiceValue c) (choiceValue c) ids]
      item "multiple" "directedPair" Nothing $
        [ el
            "matchInteraction"
            [("responseIdentifier", "RESPONSE"), ("shuffle", "false"), ("maxAssociations", "0"), ("class", "matrix")]
            (prompt ++ [set rows (if sel == Single then "1" else tshow (length cols)), set cols (tshow (length rows))])
        ]
    QScale lo hi l h -> do
      when (isJust l || isJust h) $ warn i "scale end labels dropped"
      item "single" "integer" (qDefault q) [el "sliderInteraction" [("responseIdentifier", "RESPONSE"), ("lowerBound", tshow lo), ("upperBound", tshow hi), ("step", "1")] prompt]
    QText LongText -> item "single" "string" (qDefault q) [el "extendedTextInteraction" [("responseIdentifier", "RESPONSE")] prompt]
    QText ShortText -> entry Nothing "string"
    QText Email -> entry (Just "email") "string"
    QText Url -> entry (Just "url") "string"
    QText Phone -> entry (Just "phone") "string"
    QNumber Integer -> entry Nothing "integer"
    QNumber Decimal -> entry Nothing "float"
    QDate Date -> entry (Just "date") "string"
    QDate Time -> entry (Just "time") "string"
    QDate DateTime -> entry (Just "datetime") "string"
    QGeo -> entry (Just "geopoint") "string" <* warn i "geopoint written as a text entry"
    QFile -> item "single" "file" Nothing [el "uploadInteraction" [("responseIdentifier", "RESPONSE")] prompt]
    QOther t -> entry Nothing "string" <* warn i ("type " <> t <> " written as a text entry")
    QHidden -> Nothing <$ warn i "hidden value dropped (QTI has no hidden items)"
    QCalculate _ -> Nothing <$ warn i "calculation dropped"

precondition :: M.Map Text Kind -> Text -> Maybe Expr -> Conv [Node]
precondition _ _ Nothing = pure []
precondition kinds at (Just e) = case toQti kinds e of
  Right x -> pure [el "preCondition" [] [x]]
  Left why -> [] <$ warn at ("logic dropped, QTI cannot express " <> why)

toQti :: M.Map Text Kind -> Expr -> Either Text Node
toQti kinds = go
  where
    go = \case
      EBin And a b -> el "and" [] <$> mapM go (flatten And (EBin And a b))
      EBin Or a b -> el "or" [] <$> mapM go (flatten Or (EBin Or a b))
      EBin Add a b -> el "sum" [] <$> mapM go [a, b]
      EBin Mul a b -> el "product" [] <$> mapM go [a, b]
      EBin Sub a b -> el "subtract" [] <$> mapM go [a, b]
      EBin Div a b -> el "divide" [] <$> mapM go [a, b]
      EBin Neq a b -> (\x -> el "not" [] [x]) <$> go (EBin Eq a b)
      EBin op (ELit l) (ERef q) -> go (EBin (flipOp op) (ERef q) (ELit l))
      EBin op (ERef q) (ELit l) -> Right (compareLit op q l)
      EBin op a b -> (\x y -> el (opName op) [] [x, y]) <$> go a <*> go b
      ESelected q v -> Right (el "member" [] [baseValue "identifier" (identFor q v), variable q])
      EAnswered q -> Right (el "not" [] [el "isNull" [] [variable q]])
      ENot (EAnswered q) -> Right (el "isNull" [] [variable q])
      ENot x -> (\y -> el "not" [] [y]) <$> go x
      ERef q -> Right (variable q)
      ELit l -> Right (literal l)
      ECall f _ -> Left ("function " <> f)
      ERaw d src -> Left ("expression in " <> d <> " syntax: " <> src)
    flatten op = \case
      EBin o a b | o == op -> flatten op a ++ flatten op b
      x -> [x]
    variable q = el "variable" [("identifier", q <> ".RESPONSE")] []
    baseValue t v = leaf "baseValue" [("baseType", t)] v
    literal = \case
      LNum n -> baseValue (if n == fromIntegral (round n :: Integer) then "integer" else "float") (fmtNum n)
      LBool b -> baseValue "boolean" (if b then "true" else "false")
      LStr s -> baseValue "string" s
    compareLit op q l = case (M.lookup q kinds, l) of
      (Just (KChoice Multiple _), LStr v) | op == Eq -> el "member" [] [baseValue "identifier" (identFor q v), variable q]
      (Just (KChoice _ ids), LStr v) | op == Eq -> el "match" [] [variable q, baseValue "identifier" (M.findWithDefault v v ids)]
      (Just (KChoice _ ids), LNum n) | op == Eq -> el "match" [] [variable q, baseValue "identifier" (M.findWithDefault (fmtNum n) (fmtNum n) ids)]
      (Just KBool, _) | op == Eq -> el "match" [] [variable q, baseValue "identifier" (boolIdent l)]
      (_, LNum _) | op == Eq -> el "equal" [("toleranceMode", "exact")] [variable q, literal l]
      _ -> el (opName op) [] [variable q, literal l]
    boolIdent = \case
      LBool b -> if b then "true" else "false"
      LNum n -> if n /= 0 then "true" else "false"
      LStr s -> if T.toLower s `elem` ["1", "true", "yes", "y"] then "true" else "false"
    identFor q v = case M.lookup q kinds of
      Just (KChoice _ ids) -> M.findWithDefault v v ids
      _ -> v
    opName = \case
      Eq -> "match"
      Lt -> "lt"
      Le -> "lte"
      Gt -> "gt"
      Ge -> "gte"
      And -> "and"
      Or -> "or"
      Add -> "sum"
      Sub -> "subtract"
      Mul -> "product"
      Div -> "divide"
      Neq -> "match"
    flipOp = \case
      Lt -> Gt
      Gt -> Lt
      Le -> Ge
      Ge -> Le
      o -> o
