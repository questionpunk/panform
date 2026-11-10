-- | DDI Codebook 2.5 (ddialliance.org), the archive deposit format used by
-- ICPSR, UKDS and NADA. It is variable-centric: each @var@ in @dataDscr@
-- carries a label, a @qstn@ (@qstnLit@, @preQTxt@, @ivuInstr@) and coded
-- @catgry@ values; @varGrp@ ties variables into multiple-response sets,
-- grids and sections. Logic exists only as descriptive @universe@ text.
-- (DDI-Lifecycle 3.x, the instrument-centric sibling, is not covered.)
module Panform.Format.DDI (readDDI, writeDDI) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Format.Codebook
import Panform.XmlKit

------------------------------------------------------------------------------
-- Reading

-- | A @var@ with the pieces the reader needs.
data DVar = DVar
  { dvIx :: Int
  , dvId :: Text
  , dvName :: Text
  , dvLabl :: Maybe I18n
  , dvLit :: Maybe I18n
  , dvPre :: Maybe Text
  , dvPost :: Maybe Text
  , dvInstr :: Maybe I18n
  , dvDomain :: Text
  , dvIntrvl :: Text
  , dvCats :: [(Text, I18n)]
  -- ^ Codes and labels; a category without @catValu@ is numbered.
  , dvUncoded :: Bool
  , dvRange :: (Maybe Double, Maybe Double)
  , dvFmtType :: Text
  , dvFmtName :: Text
  , dvFmtCat :: Text
  , dvUniverse :: Maybe Text
  , dvDerivation :: Maybe (Text, Text)
  , dvHasQstn :: Bool
  }

data DGroup = DGroup
  { dgId :: Text
  , dgName :: Text
  , dgType :: Text
  , dgVars :: [Text]
  , dgGroups :: [Text]
  , dgLabel :: Maybe I18n
  }

readDDI :: BL.ByteString -> Conv Form
readDDI bs = do
  root <- parseXml "DDI Codebook XML" bs
  unless (localName root == "codeBook") $ failWith "DDI: root element is not <codeBook>"
  let defLang = langOf root `orElse` attrMaybe "xml-lang" root
      stdy = child "stdyDscr" root
      titlStmt = stdy >>= child "citation" >>= child "titlStmt"
      dataDscr = child "dataDscr" root
      vars = zipWith (readVar defLang) [0 ..] (maybe [] (children "var") dataDscr)
      groups = map (readGroup defLang) (maybe [] (children "varGrp") dataDscr)
      fileName = listToMaybe [n | f <- children "fileDscr" root, Just ft <- [child "fileTxt" f], Just fn <- [child "fileName" ft], Just n <- [nonBlank (textOf fn)]]
  items <- assemble vars groups
  let ident = titlStmt >>= child "IDNo" >>= nonBlank . textOf
      title = titlStmt >>= readLangs defLang . children "titl"
      -- the file name the writer uses when none was read
      defaultFile = fromMaybe (maybe "survey" (slug . i18nDefault) title) ident
      meta =
        emptyMeta
          { metaId = ident
          , metaTitle = title
          , metaDescription = stdy >>= child "stdyInfo" >>= readLangs defLang . children "abstract"
          , metaLanguages = maybe [] (: others defLang items) defLang
          , metaAttrs = M.fromList [("ddi:fileName", String n) | Just n <- [fileName], n /= defaultFile]
          }
  pure (Form meta items)
  where
    orElse a b = maybe b Just a
    others defLang items = filter ((/= defLang) . Just) (M.keys (M.fromList [(l, ()) | ItemQuestion q <- allItems items, l <- i18nLangs (qLabel q)]))

readVar :: Maybe Lang -> Int -> Element -> DVar
readVar defLang ix v =
  let qstn = child "qstn" v
      q n = maybe [] (children n) qstn
      fmt = child "varFormat" v
      rng = listToMaybe (maybe [] (children "range") (child "valrng" v))
   in DVar
        { dvIx = ix
        , dvId = fromMaybe ("#" <> tshow ix) (nonBlank (attr "ID" v) `orElse` nonBlank (attr "name" v))
        , dvName = fromMaybe (attr "ID" v) (nonBlank (attr "name" v))
        , dvLabl = readLangs defLang (children "labl" v)
        , dvLit = readLangs defLang (q "qstnLit")
        , dvPre = listToMaybe (map exactText (q "preQTxt")) >>= nonBlank
        , dvPost = listToMaybe (map exactText (q "postQTxt")) >>= nonBlank
        , dvInstr = readLangs defLang (q "ivuInstr")
        , dvDomain = maybe "" (attr "responseDomainType") qstn
        , dvIntrvl = attr "intrvl" v
        , dvCats = [(maybe (tshow n) (trimLayout . exactText) (child "catValu" c), fromMaybe (plain "") (readLangs defLang (children "labl" c))) | (n, c) <- zip [1 :: Int ..] (children "catgry" v)]
        , dvUncoded = any (isNothing . child "catValu") (children "catgry" v)
        , dvRange = (rng >>= \r -> readNum (attr "min" r), rng >>= \r -> readNum (attr "max" r))
        , dvFmtType = maybe "" (attr "type") fmt
        , dvFmtName = maybe "" (attr "formatname") fmt
        , dvFmtCat = maybe "" (attr "category") fmt
        , dvUniverse = listToMaybe (map exactText (children "universe" v)) >>= nonBlank
        , dvDerivation = listToMaybe [(attr "syntax" c, exactText c) | d <- children "derivation" v, c <- children "drvcmd" d]
        , dvHasQstn = isJust qstn
        }
  where
    orElse a b = maybe b Just a

readGroup :: Maybe Lang -> Element -> DGroup
readGroup defLang g =
  DGroup
    { dgId = attr "ID" g
    , dgName = fromMaybe (attr "ID" g) (nonBlank (attr "name" g))
    , dgType = T.toLower (attr "type" g)
    , dgVars = T.words (attr "var" g)
    , dgGroups = T.words (attr "varGrp" g)
    , dgLabel = readLangs defLang (children "labl" g) `orElse` readLangs defLang (children "txt" g)
    }
  where
    orElse a b = maybe b Just a

-- | Units (standalone variables and multiple-response / grid sets) placed
-- into the structural groups (sections), ordered by first variable.
assemble :: [DVar] -> [DGroup] -> Conv [Item]
assemble vars groups = do
  let byId = M.fromList [(dvId v, v) | v <- vars]
      questionGroups = [g | g <- groups, dgType g `elem` ["multipleresp", "grid"]]
      structural = [g | g <- groups, dgType g `elem` ["section", "display", "repetition"]]
      ignored = [g | g <- groups, dgType g `notElem` ["multipleresp", "grid", "section", "display", "repetition"]]
      -- variable id -> the unit (question) it belongs to
      owner = M.fromList ([(dvId v, "v:" <> dvId v) | v <- vars] ++ [(vid, "g:" <> dgId g) | g <- questionGroups, vid <- dgVars g])
      unitOf vid = M.findWithDefault ("v:" <> vid) vid owner
  mapM_ (\g -> warn (dgName g) ("variable group of type " <> dgType g <> " dropped")) ignored
  units <-
    M.fromList . catMaybes
      <$> sequence
        ( [(\i -> ("g:" <> dgId g, (pos, i))) <$$> groupQuestion byId g | g <- questionGroups, let pos = minimum (maxBound : mapMaybe (fmap dvIx . (`M.lookup` byId)) (dgVars g))]
            ++ [(\i -> ("v:" <> dvId v, (dvIx v, i))) <$$> (Just <$> varQuestion v) | v <- vars, M.findWithDefault "" (dvId v) owner == "v:" <> dvId v]
        )
  let numbered = zip [0 :: Int ..] structural
      gById = M.fromList [(dgId g, ng) | ng@(_, g) <- numbered]
      childGroups = M.fromList [(c, ()) | g <- structural, c <- dgGroups g]
      -- Each structural group is placed once, where it is first reached:
      -- expanding one again under every group that lists it would grow
      -- without bound for groups that share children, and never end for
      -- groups that list each other.
      place :: (Int, DGroup) -> State (M.Map Int (), [Text]) [(Int, Item, Text)]
      place (n, g) = do
        placed <- gets (M.member n . fst)
        if placed
          then [] <$ modify' (\(seen, again) -> (seen, dgName g : again))
          else do
            modify' (\(seen, again) -> (M.insert n () seen, again))
            subs <- concat <$> mapM place (mapMaybe (`M.lookup` gById) (dgGroups g))
            let own = [(p, i, u) | u <- ordNub (map unitOf (dgVars g)), Just (p, i) <- [M.lookup u units]]
                kids = sortOn (\(p, _, _) -> p) (own ++ subs)
            pure
              [ ( minimum (maxBound : [p | (p, _, _) <- kids])
                , ItemGroup (Group (dgName g) (kindOf (dgType g)) (dgLabel g) Nothing [i | (_, i, _) <- kids] M.empty)
                , dgId g
                )
              ]
      -- groups no other group lists come first; one left over is only
      -- reachable through a loop, and is placed at the top level
      unreached ng@(n, g) = do
        placed <- gets (M.member n . fst)
        if placed then pure ([], []) else (\xs -> (xs, [dgName g])) <$> place ng
      ((tops, looped), (_, again)) = flip runState (M.empty, []) $ do
        roots <- concat <$> mapM place [ng | ng@(_, g) <- numbered, not (M.member (dgId g) childGroups)]
        rest <- mapM unreached numbered
        pure (roots ++ concatMap fst rest, concatMap snd rest)
      claimed = M.fromList [(unitOf vid, ()) | g <- structural, vid <- dgVars g]
      loose = [(p, i, u) | (u, (p, i)) <- M.toList units, not (M.member u claimed)]
  mapM_ (\n -> warn n "variable group under no top-level group (groups list each other in a loop, or share an ID); placed at the top level") looped
  mapM_ (\n -> warn n "variable group listed more than once, kept where it first appears") (ordNub (reverse again))
  pure [i | (_, i, _) <- sortOn (\(p, _, _) -> p) (tops ++ loose)]
  where
    kindOf = \case
      "section" -> Page
      "repetition" -> Repeat
      _ -> Section
    f <$$> m = fmap (fmap f) m
    -- 'nub' without its quadratic cost
    ordNub :: [Text] -> [Text]
    ordNub = go M.empty
      where
        go _ [] = []
        go seen (x : xs)
          | M.member x seen = go seen xs
          | otherwise = x : go (M.insert x () seen) xs

-- | A multiple-response set or grid as one question.
groupQuestion :: M.Map Text DVar -> DGroup -> Conv (Maybe Item)
groupQuestion byId g = case mapMaybe (`M.lookup` byId) (dgVars g) of
  [] -> Nothing <$ warn (dgName g) "variable group has no variables"
  members@(m : _) -> do
    let code v = fromMaybe (dvName v) (T.stripPrefix (dgName g <> "_") (dvName v))
        rowLabel v = fromMaybe (plain (dvName v)) (dvLit v `orElse` dvLabl v)
        label = fromMaybe (plain "") (dgLabel g `orElse` (plain <$> dvPre m))
        qt
          | dgType g == "grid" = QMatrix (if dvDomain m == "multiple" then Multiple else Single) [Choice (code v) (rowLabel v) False False | v <- members] [choiceOf c | c <- dvCats m]
          | otherwise = QChoice Multiple Radio [Choice (code v) (rowLabel v) False False | v <- members]
    pure . Just . ItemQuestion $
      (question (dgName g) "" qt)
        { qLabel = label
        , qHint = dvInstr m
        , qRelevant = universe m >>= either (const Nothing) Just
        , qAttrs = M.fromList [("ddi:universe", String u) | Just (Left u) <- [universe m]]
        }
  where
    orElse a b = maybe b Just a

choiceOf :: (Text, I18n) -> Choice
choiceOf (c, l) = Choice c l False False

-- | Universe text we wrote is REDCap-style logic; anything else is prose.
universe :: DVar -> Maybe (Either Text Expr)
universe v = do
  u <- dvUniverse v
  pure $ case parseExpr redcap u of
    Right e | not (null (refs e)) -> Right e
    _ -> Left u

varQuestion :: DVar -> Conv Item
varQuestion v = do
  let n = dvName v
  when (dvUncoded v) $ warn n "categories without codes numbered from 1"
  let label = fromMaybe (plain n) (dvLit v `orElse` dvLabl v)
      (lo, hi) = dvRange v
      range = if isJust lo || isJust hi then Just (Validation lo hi Nothing Nothing) else Nothing
      choices = map choiceOf (dvCats v)
      numKind = if dvIntrvl v == "contin" then Decimal else Integer
  qt <- case (dvDerivation v, dvHasQstn v) of
    (Just (syntax, cmd), _) -> pure (QCalculate (if T.toLower syntax `elem` ["", "redcap"] then parseOrRaw redcap cmd else ERaw syntax cmd))
    (Nothing, False) -> do
      unless (null choices) $ warn n "categories of a variable with no question dropped"
      pure QHidden
    _ -> case (dvFmtName v, dvDomain v) of
      ("boolean", _) -> pure QBoolean
      ("long", _) -> pure (QText LongText)
      ("email", _) -> pure (QText Email)
      ("url", _) -> pure (QText Url)
      ("phone", _) -> pure (QText Phone)
      ("file", _) -> pure QFile
      ("datetime", _) -> pure (QDate DateTime)
      (_, "text") -> pure (QText ShortText)
      (_, "numeric") -> pure (QNumber numKind)
      (_, "datetime") -> pure (QDate (if dvFmtCat v == "time" then Time else Date))
      (_, "geographic") -> pure QGeo
      (_, "multiple") -> pure (QChoice Multiple Radio choices)
      (_, d)
        | d `elem` ["category", "code"] || not (null choices) -> pure (fromMaybe (QChoice Single Radio choices) (scaleFromCodes (dvCats v)))
        | dvFmtType v == "numeric" -> pure (QNumber numKind)
        | d == "" -> pure (QText ShortText)
        | otherwise -> QOther d <$ warn n ("response domain " <> d <> " kept by name")
  let attrs =
        [("ddi:labl", String (i18nDefault l)) | Just l <- [dvLabl v], isJust (dvLit v), dvLit v /= Just l, dvHasQstn v]
          ++ [("ddi:preQTxt", String p) | Just p <- [dvPre v]]
          ++ [("ddi:postQTxt", String p) | Just p <- [dvPost v]]
          ++ [("ddi:universe", String u) | Just (Left u) <- [universe v]]
  pure . ItemQuestion $
    (question n "" qt)
      { qLabel = label
      , qHint = dvInstr v
      , qRelevant = universe v >>= either (const Nothing) Just
      , qValidation = case qt of
          QNumber _ -> range
          _ -> Nothing
      , qAttrs = M.fromList attrs
      }
  where
    orElse a b = maybe b Just a

------------------------------------------------------------------------------
-- Writing

-- | Output under construction, in document order: structural groups
-- (rendered at the end, once their members are known), multiple-response
-- and grid sets, and variables tagged with their innermost group.
data Out = Out
  { outGroups :: [Either (Text, Maybe Text, Group) Node]
  , outVars :: [(Text, Maybe Text, Node)]
  }

instance Semigroup Out where
  Out a b <> Out c d = Out (a ++ c) (b ++ d)

instance Monoid Out where
  mempty = Out [] []

data Counter = Counter {cVar :: Int, cGrp :: Int}

writeDDI :: Form -> Conv BL.ByteString
writeDDI (Form meta items) = do
  let defLang = listToMaybe (metaLanguages meta)
  (out, _) <- walk defLang Nothing (Counter 1 1) items
  let n = length (outVars out)
      fileName = case M.lookup "ddi:fileName" (metaAttrs meta) of
        Just (String f) -> f
        _ -> fromMaybe (maybe "survey" (slug . i18nDefault) (metaTitle meta)) (metaId meta)
      titlStmt =
        el "titlStmt" [] $
          maybe [leaf "titl" [] "Survey"] (langNodes defLang "titl" []) (metaTitle meta)
            ++ [leaf "IDNo" [] i | Just i <- [metaId meta]]
      stdy =
        el "stdyDscr" [] $
          [el "citation" [] [titlStmt]]
            ++ [el "stdyInfo" [] (langNodes defLang "abstract" [] d) | Just d <- [metaDescription meta]]
      file = el "fileDscr" [("ID", "F1")] [el "fileTxt" [] [leaf "fileName" [] fileName, el "dimensns" [] [leaf "varQnty" [] (tshow n)]]]
      structural = [x | Left x <- outGroups out]
      groupNode = \case
        Right node -> node
        Left (gid, _, g) ->
          let vs = [vid | (vid, Just owner, _) <- outVars out, owner == gid]
              subs = [sid | (sid, Just p, _) <- structural, p == gid]
           in el
                "varGrp"
                ( [("ID", gid), ("name", fromMaybe gid (nonBlank (groupId g))), ("type", kindName (groupKind g))]
                    ++ [("var", T.unwords vs) | not (null vs)]
                    ++ [("varGrp", T.unwords subs) | not (null subs)]
                )
                (maybe [] (langNodes defLang "labl" []) (groupTitle g))
  pure . renderXml $
    el
      "codeBook"
      ([("xmlns", "ddi:codebook:2_5"), ("version", "2.5")] ++ [("xml:lang", l) | Just l <- [defLang]])
      [stdy, file, el "dataDscr" [] (map groupNode (outGroups out) ++ [v | (_, _, v) <- outVars out])]
  where
    kindName = \case
      Page -> "section"
      Section -> "display"
      Repeat -> "repetition"

walk :: Maybe Lang -> Maybe Text -> Counter -> [Item] -> Conv (Out, Counter)
walk _ _ c [] = pure (mempty, c)
walk defLang parent c (i : is) = do
  (o, c') <- item defLang parent c i
  (rest, c'') <- walk defLang parent c' is
  pure (o <> rest, c'')

item :: Maybe Lang -> Maybe Text -> Counter -> Item -> Conv (Out, Counter)
item defLang parent c = \case
  ItemRaw (Format f) _ -> (mempty, c) <$ warn "ddi" ("raw " <> f <> " content dropped")
  ItemNote i _ _ -> (mempty, c) <$ warn i "display text dropped"
  ItemGroup g -> do
    when (isJust (groupRelevant g)) $ warn (groupId g) "group logic copied onto its questions"
    let gid = "G" <> tshow (cGrp c)
    when (T.null (T.strip (groupId g))) $ warn gid "unnamed group named by its position"
    (inner, c') <- walk defLang (Just gid) (c {cGrp = cGrp c + 1}) (map (pushRelevant (groupRelevant g)) (groupItems g))
    -- a group without variables has no position in a codebook
    if null (outVars inner)
      then (mempty, c) <$ warn (groupId g) "group without variables dropped"
      else pure (Out [Left (gid, parent, g)] [] <> inner, c')
  ItemQuestion q -> do
    (o, c') <- question' defLang c q
    pure (o {outVars = [(vid, parent, v) | (vid, _, v) <- outVars o]}, c')

pushRelevant :: Maybe Expr -> Item -> Item
pushRelevant Nothing i = i
pushRelevant (Just e) i = case i of
  ItemQuestion q -> ItemQuestion q {qRelevant = Just (maybe e (EBin And e) (qRelevant q))}
  ItemGroup g -> ItemGroup g {groupRelevant = Just (maybe e (EBin And e) (groupRelevant g))}
  _ -> i

question' :: Maybe Lang -> Counter -> Question -> Conv (Out, Counter)
question' defLang c q0
  | T.null (T.strip (qId q0)) = do
      let named = "Q" <> tshow (cVar c)
      warn named "unnamed question named by its position"
      question' defLang c q0 {qId = named}
  | otherwise = question'' defLang c q0

question'' :: Maybe Lang -> Counter -> Question -> Conv (Out, Counter)
question'' defLang c q = do
  lossWarnings (Loses False True False True False) q
  when (isJust (qValidation q >>= valMessage)) $ warn (qId q) "validation message dropped"
  univ <- case M.lookup "ddi:universe" (qAttrs q) of
    Just (String u) | Nothing <- qRelevant q -> pure (Just u)
    _ -> printLogic redcap (qId q) (qRelevant q)
  let vid k = "V" <> tshow (cVar c + k)
      common = (univ, qHint q)
      one v = pure (Out [] [(vid 0, Nothing, v)], c {cVar = cVar c + 1})
      label = qLabel q
      labl = case M.lookup "ddi:labl" (qAttrs q) of
        Just (String l) -> plain l
        _ -> label
      pre = [t | Just (String t) <- [M.lookup "ddi:preQTxt" (qAttrs q)]]
      post = [t | Just (String t) <- [M.lookup "ddi:postQTxt" (qAttrs q)]]
      plainVar dom extra = var defLang (vid 0) (qId q) labl (Just (dom, pre, label, post)) common extra
  case qType q of
    QChoice Multiple _ [] -> (mempty, c) <$ warn (qId q) "multi-select with no options dropped"
    QChoice Multiple _ cs -> do
      choiceWarnings cs
      let members = [var defLang (vid k) (qId q <> "_" <> choiceValue ch) (choiceLabel ch) (Just ("multiple", [i18nDefault label], choiceLabel ch, [])) common (cats selectedCats ++ [fmtNode "numeric" Nothing Nothing]) | (k, ch) <- zip [0 ..] cs]
          grp = el "varGrp" [("ID", "G" <> vid 0), ("name", qId q), ("type", "multipleResp"), ("var", T.unwords [vid k | k <- [0 .. length cs - 1]])] (langNodes defLang "labl" [] label)
      pure (Out [Right grp] [(vid k, Nothing, m) | (k, m) <- zip [0 ..] members], c {cVar = cVar c + length cs})
    QMatrix _ [] _ -> (mempty, c) <$ warn (qId q) "matrix with no rows dropped"
    QMatrix sel rows cols -> do
      let dom = if sel == Multiple then "multiple" else "category"
          members = [var defLang (vid k) (qId q <> "_" <> choiceValue r) (choiceLabel r) (Just (dom, [i18nDefault label], choiceLabel r, [])) common (cats [(choiceValue x, choiceLabel x) | x <- cols] ++ [fmtNode (codeType cols) Nothing Nothing]) | (k, r) <- zip [0 ..] rows]
          grp = el "varGrp" [("ID", "G" <> vid 0), ("name", qId q), ("type", "grid"), ("var", T.unwords [vid k | k <- [0 .. length rows - 1]])] (langNodes defLang "labl" [] label)
      pure (Out [Right grp] [(vid k, Nothing, m) | (k, m) <- zip [0 ..] members], c {cVar = cVar c + length rows})
    QRanking _ -> do
      expanded <- expandQuestion q
      foldQs c expanded
    QChoice Single style cs -> do
      choiceWarnings cs
      when (style /= Radio) $ warn (qId q) (T.toLower (tshow style) <> " display style dropped")
      one (plainVar "category" (intrvl "discrete" : cats [(choiceValue x, choiceLabel x) | x <- cs] ++ [fmtNode (codeType cs) Nothing Nothing]))
    QScale lo hi l h -> do
      when (scaleTooWide lo hi) $ warn (qId q) ("scale from " <> tshow lo <> " to " <> tshow hi <> " written as its first " <> tshow maxScalePoints <> " points")
      one (plainVar "category" (intrvl "discrete" : cats (scaleCodes lo hi l h) ++ [fmtNode "numeric" Nothing Nothing]))
    QBoolean -> one (plainVar "category" (intrvl "discrete" : cats [("1", plain "Yes"), ("0", plain "No")] ++ [fmtNode "numeric" (Just "boolean") Nothing]))
    QText k -> one (plainVar "text" [fmtNode "character" (textFmt k) Nothing])
    QNumber k -> one (plainVar "numeric" (intrvl (if k == Decimal then "contin" else "discrete") : valrng ++ [fmtNode "numeric" Nothing Nothing]))
    QDate Date -> one (plainVar "datetime" [fmtNode "character" Nothing (Just "date")])
    QDate Time -> one (plainVar "datetime" [fmtNode "character" Nothing (Just "time")])
    QDate DateTime -> one (plainVar "datetime" [fmtNode "character" (Just "datetime") (Just "date")])
    QGeo -> one (plainVar "geographic" [fmtNode "character" Nothing Nothing])
    QFile -> one (plainVar "other" [fmtNode "character" (Just "file") Nothing])
    QOther t -> do
      warn (qId q) ("type " <> t <> " written as a text variable")
      one (plainVar "text" [fmtNode "character" Nothing Nothing])
    QHidden -> do
      when (isJust (qHint q)) $ warn (qId q) "hint dropped (a variable with no question has no instructions)"
      one (var defLang (vid 0) (qId q) label Nothing (univ, Nothing) [fmtNode "character" Nothing Nothing])
    QCalculate e -> do
      cmd <- printLogic redcap (qId q) (Just e)
      -- without its formula it is written exactly like a hidden value
      when (isJust (qHint q)) $ warn (qId q) "hint dropped (a variable with no question has no instructions)"
      one (var defLang (vid 0) (qId q) label Nothing (univ, Nothing) (maybe [fmtNode "character" Nothing Nothing] (\t -> [Deriv (el "derivation" [] [leaf "drvcmd" [("syntax", "redcap")] t]), fmtNode "numeric" Nothing Nothing]) cmd))
  where
    selectedCats = [("0", plain "Not selected"), ("1", plain "Selected")]
    cats cs = [Cat code l | (code, l) <- cs]
    intrvl = Intrvl
    valrng = case qValidation q of
      Just v | isJust (valMin v) || isJust (valMax v) -> [Range (valMin v) (valMax v)]
      _ -> []
    textFmt = \case
      ShortText -> Nothing
      LongText -> Just "long"
      Email -> Just "email"
      Url -> Just "url"
      Phone -> Just "phone"
    codeType cs = if all (isNumCode . choiceValue) cs then "numeric" else "character"
    isNumCode t = isJust (readNum t)
    choiceWarnings cs = do
      when (any choiceOther cs) $ warn (qId q) "\"other, specify\" text box dropped"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive (none of the above) flag dropped"
    foldQs c0 [] = pure (mempty, c0)
    foldQs c0 (x : xs) = do
      (o, c1) <- question' defLang c0 x
      (o', c2) <- foldQs c1 xs
      pure (o <> o', c2)

-- | The parts of a @var@ that follow @qstn@, in schema order.
data Part = Intrvl Text | Range (Maybe Double) (Maybe Double) | Cat Text I18n | Fmt Node | Deriv Node

fmtNode :: Text -> Maybe Text -> Maybe Text -> Part
fmtNode ty name cat =
  Fmt (el "varFormat" ([("type", ty)] ++ [("schema", "other") | isJust name] ++ [("formatname", n) | Just n <- [name]] ++ [("category", x) | Just x <- [cat]]) [])

-- | A @var@: labl, qstn, valrng, universe, catgry, derivation, varFormat.
var :: Maybe Lang -> Text -> Text -> I18n -> Maybe (Text, [Text], I18n, [Text]) -> (Maybe Text, Maybe I18n) -> [Part] -> Node
var defLang vid name labl qstn (univ, hint) parts =
  el "var" ([("ID", vid), ("name", name), ("files", "F1")] ++ [("intrvl", i) | Intrvl i <- parts]) $
    langNodes defLang "labl" [] labl
      ++ [ el "qstn" [("responseDomainType", dom)] $
             [leaf "preQTxt" [] p | p <- pre, not (T.null p)]
               ++ langNodes defLang "qstnLit" [] lit
               ++ [leaf "postQTxt" [] p | p <- post]
               ++ maybe [] (langNodes defLang "ivuInstr" []) hint
         | Just (dom, pre, lit, post) <- [qstn]
         ]
      ++ [el "valrng" [] [el "range" ([("min", fmtNum x) | Just x <- [lo]] ++ [("max", fmtNum x) | Just x <- [hi]]) []] | Range lo hi <- parts]
      ++ [leaf "universe" [("clusion", "I")] u | Just u <- [univ]]
      ++ [el "catgry" [] (leaf "catValu" [] code : langNodes defLang "labl" [] l) | Cat code l <- parts]
      ++ [d | Deriv d <- parts]
      ++ [f | Fmt f <- parts]
