-- | CDISC ODM 1.3.2 study metadata (@Study/MetaDataVersion@): @FormDef@s
-- (pages) reference @ItemGroupDef@s (sections), which reference
-- @ItemDef@s with a @Question@, a @DataType@, @RangeCheck@s and a
-- @CodeListRef@. A collection-exception @ConditionDef@ says when an item is
-- /not/ asked. REDCap's project XML is ODM plus @redcap:*@ attributes
-- (field type, branching logic, notes, matrix groups, checkbox options),
-- which are read when present and always written.
module Panform.Format.ODM (readODM, writeODM) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.List (groupBy, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Format.Codebook
import Panform.XmlKit

------------------------------------------------------------------------------
-- Reading

data Env = Env
  { eLang :: Maybe Lang
  , eCodeLists :: M.Map Text Element
  , eConditions :: M.Map Text Element
  , eItems :: M.Map Text Element
  , eRedcap :: Bool
  -- ^ REDCap field attributes are present.
  , eFromRedcap :: Bool
  -- ^ Exported by REDCap itself, whose range messages are boilerplate.
  , eOidName :: M.Map Text Text
  -- ^ Item OID to variable name, for rewriting condition references.
  }

-- | One ItemDef as read, before checkbox options and matrix rows merge.
data Field = Field
  { fItem :: Item
  , fMatrix :: Maybe Text
  , fHeader :: Maybe Text
  , fCheckbox :: Maybe (Text, [Choice])
  -- ^ The checkbox variable this option belongs to, with all its options.
  }

readODM :: BL.ByteString -> Conv Form
readODM bs = do
  root <- parseXml "CDISC ODM XML" bs
  unless (localName root == "ODM") $ failWith "ODM: root element is not <ODM>"
  study <- maybe (failWith "ODM: missing <Study>") pure (child "Study" root)
  mdv <- maybe (failWith "ODM: missing <MetaDataVersion>") pure (child "MetaDataVersion" study)
  let byOid n = M.fromList [(attr "OID" e, e) | e <- children n mdv]
      items = byOid "ItemDef"
      -- the language of each question's first text is the default
      firstLang = listToMaybe [l | i <- children "ItemDef" mdv, q <- children "Question" i, t : _ <- [children "TranslatedText" q], Just l <- [langOf t]]
      env =
        Env
          { eLang = firstLang
          , eCodeLists = byOid "CodeList"
          , eConditions = byOid "ConditionDef"
          , eItems = items
          , eRedcap = attr "SourceSystem" root == "REDCap" || any (isJust . attrMaybe "FieldType") (M.elems items)
          , eFromRedcap = attr "SourceSystem" root == "REDCap"
          , eOidName = M.map itemName items
          }
      groups = byOid "ItemGroupDef"
      gv = child "GlobalVariables" study
      events = children "StudyEventDef" mdv
  when (length events > 1) $ warn "odm" "study events and arms flattened; each form is listed once"
  let formDefs = children "FormDef" mdv
  v2 <-
    if null formDefs && not (M.null groups)
      then Just <$> odm2Forms env mdv groups
      else pure Nothing
  forms1 <- forM formDefs $ \f -> do
    let fid = fromMaybe (attr "OID" f) (nonBlank (attr "FormName" f) `orElse` T.stripPrefix "Form." (attr "OID" f))
        fname = attr "Name" f
        refs' = mapMaybe (\r -> M.lookup (attr "ItemGroupOID" r) groups) (children "ItemGroupRef" f)
    kids <-
      if eFromRedcap env
        then redcapForm env fid refs'
        else concat <$> mapM (readGroup env fid fname) refs'
    pure . ItemGroup $ Group fid Page (describe env f) Nothing kids M.empty
  let forms = fromMaybe forms1 v2
      allLangs = nub [l | ItemGroup g <- forms, ItemQuestion q <- allItems (groupItems g), l <- i18nLangs (qLabel q)]
      meta =
        emptyMeta
          { metaId = nonBlank (attr "FileOID" root)
          , metaTitle = plain <$> ((gv >>= child "StudyName" >>= nonBlank . exactText) `orElse` nonBlank (attr "StudyName" study))
          , metaDescription = plain <$> (gv >>= child "StudyDescription" >>= nonBlank . exactText)
          , metaLanguages = maybe [] (\l -> l : filter (/= l) allLangs) firstLang
          , metaAttrs = M.fromList [("odm:CreationDateTime", String t) | Just t <- [nonBlank (attr "CreationDateTime" root)]]
          }
  pure (Form meta forms)
  where
    orElse a b = maybe b Just a

-- | The most item and item group references an ODM 2.0 document may expand
-- to (a group referenced twice counts twice).
maxExpandedRefs :: Int
maxExpandedRefs = 200000

-- | ODM 2.0 has no FormDef: an ItemGroupDef of @Type="Form"@ (else one
-- no other group references) is a page, and item groups nest by
-- @ItemGroupRef@ among their @ItemRef@s.
odm2Forms :: Env -> Element -> M.Map Text Element -> Conv [Item]
odm2Forms env mdv groups = do
  let all' = children "ItemGroupDef" mdv
      referenced = M.fromList [(attr "ItemGroupOID" r, ()) | g <- all', r <- children "ItemGroupRef" g]
      typed = [g | g <- all', T.toLower (attr "Type" g) == "form"]
      tops = if null typed then [g | g <- all', not (M.member (attr "OID" g) referenced)] else typed
      top (acc, budget) g = do
        (kids, left) <- contents budget (M.singleton (attr "OID" g) ()) g
        pure (ItemGroup (Group (attr "OID" g) Page (describe env g) Nothing kids M.empty) : acc, left)
  reverse . fst <$> foldM top ([], maxExpandedRefs) tops
  where
    -- A group may be referenced from several places and is read again at
    -- each, so the references followed are counted against a limit: groups
    -- that share children would otherwise expand without bound.
    contents budget seen g = do
      (parts, left) <- foldM (part seen) ([], budget) (childrenAny ["ItemRef", "ItemGroupRef"] g)
      pure (runs (concat (reverse parts)), left)
    part seen (acc, budget) r
      | budget <= 0 = failWith ("ODM: item group references expand to more than " <> tshow maxExpandedRefs <> " items and groups")
      | localName r == "ItemRef" = (\f -> (maybe [] (pure . Left) f : acc, budget - 1)) <$> readRef env r
      | otherwise = case M.lookup (attr "ItemGroupOID" r) groups of
          Just sub | not (M.member (attr "OID" sub) seen) -> do
            (kids, left) <- contents (budget - 1) (M.insert (attr "OID" sub) () seen) sub
            let kind = if attr "Repeating" sub `elem` ["Yes", "Simple", "Dynamic", "Static"] then Repeat else Section
            pure ([Right (ItemGroup (Group (attr "OID" sub) kind (describe env sub) Nothing kids M.empty))] : acc, left)
          _ -> (acc, budget - 1) <$ warn (attr "ItemGroupOID" r) "item group reference not resolved"
    -- consecutive fields merge into multi-selects and matrices
    runs = \case
      [] -> []
      Right i : rest -> i : runs rest
      xs ->
        let (fs, rest) = span isLeft xs
         in mergeMatrices "" (mergeCheckboxes [f | Left f <- fs]) ++ runs rest
    isLeft = \case
      Left _ -> True
      Right _ -> False

-- | A form or group title: its translated @Description@, else its name.
describe :: Env -> Element -> Maybe I18n
describe env e = (child "Description" e >>= readLangs (eLang env) . children "TranslatedText") `orElse` (plain <$> nonBlank (attr "Name" e))
  where
    orElse a b = maybe b Just a

itemName :: Element -> Text
itemName i = fromMaybe (attr "OID" i) (nonBlank (attr "Name" i))

-- | An ItemGroupDef named like its form (or unnamed) holds the form's own
-- items; any other becomes a section titled with its name. REDCap's
-- "Form Status" group is bookkeeping and skipped.
readGroup :: Env -> Text -> Text -> Element -> Conv [Item]
readGroup env fid fname g = do
  let name = attr "Name" g
      refs' = children "ItemRef" g
  if eRedcap env && name == "Form Status"
    then [] <$ warn fid "REDCap form status field skipped"
    else do
      fields <- catMaybes <$> mapM (readRef env) refs'
      let items = mergeMatrices "" (mergeCheckboxes fields)
          gid = fromMaybe (attr "OID" g) (T.stripPrefix (fid <> ".") (attr "OID" g))
          repeating = attr "Repeating" g == "Yes"
      pure $
        if (T.null (T.strip name) || name == fname) && not repeating
          then items
          else [ItemGroup (Group gid (if repeating then Repeat else Section) (describe env g) Nothing items M.empty)]

-- | REDCap splits a form into item groups arbitrarily; its sections are
-- the @SectionHeader@s, each running to the next, as in the data
-- dictionary.
redcapForm :: Env -> Text -> [Element] -> Conv [Item]
redcapForm env fid gs = do
  let (status, rest) = span' (\g -> attr "Name" g == "Form Status") gs
  unless (null status) $ warn fid "REDCap form status field skipped"
  fields <- mergeCheckboxes . catMaybes <$> mapM (readRef env) (concatMap (children "ItemRef") rest)
  pure (go fields)
  where
    span' p xs = (filter p xs, filter (not . p) xs)
    go [] = []
    go (f : fs) = case fHeader f of
      Just h ->
        let (inSec, after) = break (isJust . fHeader) fs
         in ItemGroup (Group ("section_" <> fieldId f) Section (Just (plain h)) Nothing (mergeMatrices h (f {fHeader = Nothing} : inSec)) M.empty) : go after
      Nothing ->
        let (loose, after) = break (isJust . fHeader) (f : fs)
         in mergeMatrices "" loose ++ go after
    fieldId f = case fItem f of
      ItemQuestion q -> qId q
      i -> itemId i

readRef :: Env -> Element -> Conv (Maybe Field)
readRef env r = case M.lookup (attr "ItemOID" r) (eItems env) of
  Nothing -> Nothing <$ warn (attr "ItemOID" r) "ItemRef to a missing ItemDef skipped"
  Just i
    | eRedcap env && T.null (attr "FieldType" i) -> Nothing <$ warn (itemName i) "REDCap system field skipped"
    | otherwise -> do
        when (isJust (attrMaybe "MethodOID" r)) $ warn (itemName i) "derivation method dropped"
        Just <$> readItem env (attr "Mandatory" r == "Yes") (nonBlank (attr "CollectionExceptionConditionOID" r)) i

readItem :: Env -> Bool -> Maybe Text -> Element -> Conv Field
readItem env mandatory cond i = do
  let var = fromMaybe (itemName i) (nonBlank (attr "Variable" i))
      name = itemName i
      ftype = attr "FieldType" i
      dtype = T.toLower (attr "DataType" i)
      vtype = T.toLower (attr "TextValidationType" i)
      q = child "Question" i
      label = fromMaybe (plain "") ((q >>= readLangs (eLang env) . children "TranslatedText") `orElse` (child "Description" i >>= readLangs (eLang env) . children "TranslatedText"))
      codeList = child "CodeListRef" i >>= \c -> M.lookup (attr "CodeListOID" c) (eCodeLists env)
      codes = maybe [] (codeListChoices (eLang env)) codeList
      annot = attr "FieldAnnotation" i
      checks = children "RangeCheck" i
      bound cmps = listToMaybe [v | c <- checks, attr "Comparator" c `elem` cmps, Just v <- [child "CheckValue" c >>= readNum . textOf]]
      -- a slider's ends, inclusive or not
      lo = bound ["GE", "GT"]
      hi = bound ["LE", "LT"]
      bounds = [(cmp, v) | c <- checks, let cmp = attr "Comparator" c, cmp `elem` ["GE", "GT", "LE", "LT"], Just v <- [child "CheckValue" c >>= readNum . textOf]]
      message = if eFromRedcap env then Nothing else listToMaybe [m | c <- checks, Just e <- [child "ErrorMessage" c], Just m <- [readLangs (eLang env) (children "TranslatedText" e)]]
      numeric k = QNumber k
  unless (null (children "MeasurementUnitRef" i)) $ warn name "measurement unit dropped"
  unless (null [c | c <- checks, attr "Comparator" c `notElem` ["GE", "GT", "LE", "LT"]]) $ warn name "range check other than a bound dropped"
  qt <- case ftype of
    "" -> pure $ case dtype of
      _ | not (null codes), dtype /= "boolean" -> QChoice Single Radio codes
      "integer" -> numeric Integer
      "float" -> numeric Decimal
      "double" -> numeric Decimal
      "date" -> QDate Date
      "time" -> QDate Time
      "datetime" -> QDate DateTime
      "partialdatetime" -> QDate DateTime
      "partialdate" -> QDate Date
      "boolean" -> QBoolean
      "uri" -> QText Url
      _ -> QText ShortText
    "text"
      | "@hidden" `T.isInfixOf` T.toLower annot && not ("@hidden-" `T.isInfixOf` T.toLower annot) -> pure QHidden
      | dtype == "uri" -> pure (QText Url)
      | otherwise -> pure $ case vtype of
          t | t `elem` ["int", "integer"] -> numeric Integer
          t | t `elem` ["float", "number"] || "number_" `T.isPrefixOf` t -> numeric Decimal
          "email" -> QText Email
          "phone" -> QText Phone
          t | "datetime" `T.isPrefixOf` t -> QDate DateTime
          t | "date" `T.isPrefixOf` t -> QDate Date
          t | "time" `T.isPrefixOf` t -> QDate Time
          _ -> QText ShortText
    t | t `elem` ["textarea", "notes"] -> pure (QText LongText)
    "radio" -> pure (QChoice Single Radio codes)
    t | t `elem` ["select", "dropdown"] -> pure (QChoice Single Dropdown codes)
    t | t `elem` ["yesno", "truefalse"] -> pure QBoolean
    "calc" -> pure (QCalculate (parseOrRaw redcap (attr "Calculation" i)))
    "file" -> pure QFile
    "slider" -> do
      let raw = attr "SliderLabels" i
          -- ours are joined with " | " and kept exact; REDCap's are trimmed
          ls = if " | " `T.isInfixOf` raw then T.splitOn " | " raw else map T.strip (T.splitOn "|" raw)
          end x = plain <$> (x >>= nonBlank)
      when (length ls > 2 && any (not . T.null) (drop 1 (take (length ls - 1) ls))) $ warn var "slider middle label dropped"
      pure (QScale (maybe 0 round lo) (maybe 100 round hi) (end (listToMaybe ls)) (end (listToMaybe (reverse ls))))
    "checkbox" -> pure (QChoice Multiple Radio [])
    "descriptive" -> pure (QOther "descriptive")
    other -> QOther other <$ warn var ("unsupported REDCap field type " <> other)
  -- A question's range checks: the first GE and LE are its minimum and
  -- maximum. A strict bound (GT, LT) excludes its value: on a whole
  -- number it is the next value in, exactly; on any other answer it is
  -- read as inclusive, with a warning. A further bound is kept as a rule.
  let whole = qt == QNumber Integer
      inclusive (cmp, v) = case cmp of
        "GT" | whole -> ("GE", fromIntegral (floor v + 1 :: Integer))
        "LT" | whole -> ("LE", fromIntegral (ceiling v - 1 :: Integer))
        "GT" -> ("GE", v)
        "LT" -> ("LE", v)
        _ -> (cmp, v)
      bounds' = map inclusive bounds
      minV = lookup "GE" bounds'
      maxV = lookup "LE" bounds'
      firstOf cmp = listToMaybe [n | (n, (c, _)) <- zip [0 :: Int ..] bounds', c == cmp]
      rule = case [EBin (comparatorOp c) (ERef ".") (ELit (LNum v)) | (n, (c, v)) <- zip [0 :: Int ..] bounds', Just n /= firstOf c] of
        [] -> Nothing
        es -> Just (foldr1 (EBin And) es)
  unless (whole || isScale qt) $
    forM_ [(c, v) | (c, v) <- bounds, c `elem` ["GT", "LT"]] $ \(c, v) ->
      warn name ("range check " <> c <> " " <> fmtNum v <> " read as " <> (if c == "GT" then "at least " else "at most ") <> fmtNum v)
  relevant <- case nonBlank (attr "BranchingLogic" i) of
    Just b -> pure (Just (parseOrRaw redcap b))
    Nothing -> maybe (pure Nothing) (fmap Just . exceptionToRelevant env) cond
  let noneCodes = noneOf annot
      checkbox
        | ftype == "checkbox" =
            -- this item is one option: its code follows @___@ in the name,
            -- its label is the Description (ours) or REDCap's choice list
            let listed = parseChoices (maybe "" (attr "CheckboxChoices") codeList)
                code = fromMaybe name (T.stripPrefix (var <> "___") name)
                optLabel =
                  fromMaybe (plain code) $
                    (child "Description" i >>= readLangs (eLang env) . children "TranslatedText")
                      `orElse` (choiceLabel <$> listToMaybe [c | c <- listed, choiceValue c == code])
             in Just (var, [Choice code optLabel False (code `elem` noneCodes)])
        | otherwise = Nothing
      validation = case qt of
        QScale {} -> Nothing
        _ | isJust minV || isJust maxV || isJust rule || isJust message -> Just (Validation minV maxV rule message)
        _ -> Nothing
      attrs =
        M.fromList
          [ ("redcap:" <> k, String v)
          | (k, v) <- [("annotation", stripNone annot), ("identifier", attr "Identifier" i), ("alignment", attr "CustomAlignment" i)]
          , not (T.null v)
          ]
      question' =
        (question var "" qt)
          { qLabel = label
          , qHint = plain <$> nonBlank (attr "FieldNote" i)
          , qRequired = mandatory || attr "RequiredField" i == "y"
          , qRelevant = relevant
          , qValidation = validation
          , qAttrs = attrs
          }
      item = case qt of
        QOther "descriptive" -> ItemNote var label relevant
        _ -> ItemQuestion question'
  pure (Field item (nonBlank (attr "MatrixGroupName" i)) (nonBlank (attr "SectionHeader" i)) checkbox)
  where
    orElse a b = maybe b Just a

codeListChoices :: Maybe Lang -> Element -> [Choice]
codeListChoices defLang cl =
  [ Choice (attr "CodedValue" c) (fromMaybe (plain (attr "CodedValue" c)) (child "Decode" c >>= readLangs defLang . children "TranslatedText")) False False
  | c <- childrenAny ["CodeListItem", "EnumeratedItem"] cl
  ]

-- | A collection exception says when an item is skipped; relevance is its
-- negation. REDCap-context expressions use the bracket dialect; others are
-- tried as C-style (@!(a == "x")@) and otherwise kept verbatim.
exceptionToRelevant :: Env -> Text -> Conv Expr
exceptionToRelevant env oid = case M.lookup oid (eConditions env) >>= child "FormalExpression" of
  Nothing -> ERaw "odm" oid <$ warn oid "condition without a formal expression kept by OID"
  Just fe -> do
    let ctx = attr "Context" fe
        src = textOf fe
        parsed
          | T.toLower ctx == "redcap" = parseOrRaw redcap src
          | otherwise = either (const (ERaw ctx src)) (renameRefs (eOidName env)) (parseExpr limesurvey src)
    pure $ case parsed of
      ENot x -> x
      ERaw c s -> ENot (ERaw c s)
      e -> fromMaybe (ENot e) (pushNot e)

renameRefs :: M.Map Text Text -> Expr -> Expr
renameRefs m = go
  where
    rn r = M.findWithDefault r r m
    go = \case
      ERef r -> ERef (rn r)
      ESelected r v -> ESelected (rn r) v
      EAnswered r -> EAnswered (rn r)
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      e -> e

-- | Consecutive checkbox options of one variable become one multi-select.
mergeCheckboxes :: [Field] -> [Field]
mergeCheckboxes = concatMap merge . groupBy ((==) `on` (fmap fst . fCheckbox))
  where
    merge fs@(f : _)
      | Just (_, cs) <- fCheckbox f
      , ItemQuestion q <- fItem f =
          [f {fItem = ItemQuestion q {qType = QChoice Multiple Radio (cs ++ concat [c | Field {fCheckbox = Just (_, c)} <- drop 1 fs])}, fCheckbox = Nothing}]
      | otherwise = fs
    merge [] = []

-- | Consecutive fields sharing a matrix group and choices become one
-- 'QMatrix'. The first row's section header is the matrix label unless it
-- already titles the enclosing group.
mergeMatrices :: Text -> [Field] -> [Item]
mergeMatrices groupName = concatMap merge . groupBy ((==) `on` key)
  where
    key f = fMatrix f >>= \m -> if isJust (scaleOf f) then Just m else Nothing
    scaleOf f = case fItem f of
      ItemQuestion q | QChoice s _ cs <- qType q -> Just (s, cs)
      _ -> Nothing
    merge fs@(f : _)
      | Just g <- fMatrix f
      , Just (sel, cols) <- scaleOf f
      , all ((== Just (sel, cols)) . scaleOf) fs
      , ItemQuestion q <- fItem f =
          [ ItemQuestion
              q
                { qId = g
                , qLabel = plain (maybe "" (\h -> if h == groupName then "" else h) (fHeader f))
                , qType = QMatrix sel [Choice (fromMaybe (qId r) (T.stripPrefix (g <> "_") (qId r))) (qLabel r) False False | Field {fItem = ItemQuestion r} <- fs] cols
                }
          ]
    merge fs = map fItem fs

-- | "1, Yes | 2, No" (a missing code uses the label as the code).
parseChoices :: Text -> [Choice]
parseChoices t =
  [ case T.breakOn "," part of
      (code, rest) | not (T.null rest) -> choice (T.strip code) (T.strip (T.drop 1 rest))
      _ -> choice (T.strip part) (T.strip part)
  | part <- T.splitOn "|" t
  , not (T.null (T.strip part))
  ]

-- | Codes named by a @\@NONEOFTHEABOVE='98,99'@ action tag.
noneOf :: Text -> [Text]
noneOf annot = case T.breakOn "@NONEOFTHEABOVE" (T.toUpper annot) of
  (pre, rest) | not (T.null rest) ->
    let after = T.stripStart (T.drop (T.length pre + T.length "@NONEOFTHEABOVE") annot)
        value = case T.uncons after of
          Just ('=', v) -> T.takeWhile (\c -> c /= '\'' && c /= '"') (T.dropWhile (\c -> c == '\'' || c == '"' || c == ' ') v)
          _ -> ""
     in filter (not . T.null) (map T.strip (T.splitOn "," value))
  _ -> []

-- | The annotation without its @\@NONEOFTHEABOVE='...'@ tag (quoted value
-- included), which the choices carry.
stripNone :: Text -> Text
stripNone annot = case T.breakOn "@NONEOFTHEABOVE" (T.toUpper annot) of
  (pre, rest) | not (T.null rest) ->
    let after = T.drop (T.length pre + T.length "@NONEOFTHEABOVE") annot
        rest' = case T.uncons after of
          Just ('=', v) -> case T.uncons v of
            Just (q, v') | q `elem` ['\'', '"'] -> T.drop 1 (T.dropWhile (/= q) v')
            _ -> T.dropWhile (/= ' ') v
          _ -> after
     in T.unwords (T.words (T.take (T.length pre) annot <> " " <> rest'))
  _ -> annot

------------------------------------------------------------------------------
-- Writing

-- | Definitions collected while walking the survey, in output order.
data Defs = Defs
  { dForms :: [Node]
  , dGroups :: [Node]
  , dItems :: [Node]
  , dCodeLists :: [Node]
  , dConditions :: [Node]
  }

instance Semigroup Defs where
  Defs a b c d e <> Defs a' b' c' d' e' = Defs (a ++ a') (b ++ b') (c ++ c') (d ++ d') (e ++ e')

instance Monoid Defs where
  mempty = Defs [] [] [] [] []

-- | An ItemDef under construction with its ItemRef.
data Out = Out {oRef :: Node, oDefs :: Defs}

writeODM :: Form -> Conv BL.ByteString
writeODM (Form meta items0) = do
  -- ODM identifies everything by OID: blank and repeated ids get a suffix
  let (items, renames) = uniqueIds items0
  forM_ renames $ \(old, new) -> warn (if T.null old then new else old) ("id " <> (if T.null old then "(blank)" else "repeated") <> ", written as " <> new)
  let defLang = listToMaybe (metaLanguages meta)
      title = maybe "Survey" i18nDefault (metaTitle meta)
      fileOid = fromMaybe (slug title) (metaId meta)
  when (any (not . null . i18nLangs) (catMaybes [metaTitle meta, metaDescription meta])) $
    warn "odm" "survey title and description translations dropped"
  let isPage = \case
        ItemGroup g -> groupKind g == Page
        _ -> False
      topGroups = [() | ItemGroup _ <- items]
  when (not (null topGroups) && not (all isPage items)) $
    warn "odm" "items outside a page wrapped in a form (every ODM item needs one)"
  forms <- mapM (form defLang) (pages (slug title) items)
  let defs = mconcat forms
      formRefs = [el "FormRef" [("FormOID", "Form." <> fid), ("OrderNumber", tshow n), ("Mandatory", "No")] [] | (n, El "FormDef" as _) <- zip [1 :: Int ..] (dForms defs), Just oid <- [lookup "OID" as], Just fid <- [T.stripPrefix "Form." oid]]
      mdv =
        el "MetaDataVersion" [("OID", "MDV.1"), ("Name", title)] $
          [ el "Protocol" [] [el "StudyEventRef" [("StudyEventOID", "SE.1"), ("OrderNumber", "1"), ("Mandatory", "Yes")] []]
          , el "StudyEventDef" [("OID", "SE.1"), ("Name", "Survey"), ("Repeating", "No"), ("Type", "Common")] formRefs
          ]
            ++ dForms defs
            ++ dGroups defs
            ++ dItems defs
            ++ dCodeLists defs
            ++ dConditions defs
      created = case M.lookup "odm:CreationDateTime" (metaAttrs meta) of
        Just (String t) -> t
        _ -> "1970-01-01T00:00:00"
      global =
        el "GlobalVariables" [] $
          [leaf "StudyName" [] title, leaf "StudyDescription" [] (maybe "" i18nDefault (metaDescription meta)), leaf "ProtocolName" [] title]
  pure . renderXml $
    el
      "ODM"
      [ ("xmlns", "http://www.cdisc.org/ns/odm/v1.3")
      , ("xmlns:redcap", "https://projectredcap.org")
      , ("ODMVersion", "1.3.2")
      , ("FileType", "Snapshot")
      , ("FileOID", fileOid)
      , ("CreationDateTime", created)
      , ("SourceSystem", "panform")
      ]
      [el "Study" [("OID", "Project." <> fileOid)] [global, mdv]]

-- | Top-level pages; loose items and non-page groups around them are
-- gathered into a form named after the survey.
pages :: Text -> [Item] -> [(Text, Maybe I18n, Maybe Expr, [Item])]
pages dflt = go (1 :: Int)
  where
    go _ [] = []
    go n (ItemGroup g : rest) | groupKind g == Page = (groupId g, groupTitle g, groupRelevant g, groupItems g) : go n rest
    go n is =
      let (loose, rest) = break isPage is
          fid = if n == 1 then dflt else dflt <> "_" <> tshow n
       in (fid, Nothing, Nothing, loose) : go (n + 1) rest
    isPage = \case
      ItemGroup g -> groupKind g == Page
      _ -> False

form :: Maybe Lang -> (Text, Maybe I18n, Maybe Expr, [Item]) -> Conv Defs
form defLang (fid, title, rel, kids) = do
  when (isJust rel) $ warn fid "page logic copied onto its questions"
  let fname = fromMaybe fid (title >>= nonBlank . i18nDefault)
  runs <- groupRuns fid fname (map (pushRelevant rel) kids)
  defs <- mconcat <$> mapM (itemGroup defLang) runs
  let refs' = [el "ItemGroupRef" [("ItemGroupOID", oid), ("Mandatory", "No")] [] | El "ItemGroupDef" as _ <- dGroups defs, Just oid <- [lookup "OID" as]]
  pure defs {dForms = el "FormDef" [("OID", "Form." <> fid), ("Name", fname), ("Repeating", "No"), ("redcap:FormName", fid)] (description defLang title ++ refs') : dForms defs}

-- | Blank or repeated question and group ids made unique (questions and
-- notes share one namespace, groups another), with the renames made.
uniqueIds :: [Item] -> ([Item], [(Text, Text)])
uniqueIds is =
  let (out, (_, _, rs)) = runState (mapM go is) (M.empty, M.empty, [])
   in (out, reverse rs)
  where
    go :: Item -> State (M.Map Text Int, M.Map Text Int, [(Text, Text)]) Item
    go = \case
      ItemQuestion q -> do
        i <- claim True (qId q)
        -- rows and options become items of their own (@q_row@,
        -- @q___option@), so their codes must be unique and the items they
        -- become must not take another question's id
        t <- case qType q of
          QMatrix Single rows cols -> (\rs -> QMatrix Single rs cols) <$> codes (\v -> [i <> "_" <> v]) i rows
          QMatrix Multiple rows cols -> (\rs -> QMatrix Multiple rs cols) <$> codes (\v -> [i <> "_" <> v <> "___" <> choiceValue c | c <- cols]) i rows
          QChoice Multiple st cs -> QChoice Multiple st <$> codes (\v -> [i <> "___" <> v]) i cs
          QRanking cs -> QRanking <$> codes (\v -> [i <> "_" <> v]) i cs
          t -> pure t
        pure (ItemQuestion q {qId = i, qType = t})
      ItemNote i t r -> (\i' -> ItemNote i' t r) <$> claim True i
      ItemGroup g -> do
        gid <- claim False (groupId g)
        kids <- mapM go (groupItems g)
        pure (ItemGroup g {groupId = gid, groupItems = kids})
      i -> pure i
    claim isItem i = do
      seen <- gets (\(a, b, _) -> if isItem then a else b)
      let base = if T.null (T.strip i) then (if isItem then "item" else "group") else i
          (new, seen') = fresh (const True) base seen
      modify' (\(a, b, rs) -> (if isItem then seen' else a, if isItem then b else seen', if new /= i then (i, new) : rs else rs))
      pure new
    codes expand qid cs = do
      (_, fixed) <- foldM pick (M.empty, []) cs
      pure (reverse fixed)
      where
        pick (seen, acc) c = do
          taken <- gets (\(a, _, _) -> a)
          let base = if T.null (T.strip (choiceValue c)) then "blank" else choiceValue c
              (v, seen') = fresh (\x -> not (any (`M.member` taken) (expand x))) base seen
          modify' (\(a, b, rs) -> (foldr (\o -> M.insertWith (\_ old -> old) o 2) a (expand v), b, if v /= choiceValue c then (qid <> "/" <> choiceValue c, qid <> "/" <> v) : rs else rs))
          pure (seen', c {choiceValue = v} : acc)
    -- The first of base, base_2, base_3, ... that is not taken and passes
    -- the check. Each taken name remembers the next suffix to try, so many
    -- repeats of one name do not rescan the suffixes already given out.
    fresh :: (Text -> Bool) -> Text -> M.Map Text Int -> (Text, M.Map Text Int)
    fresh ok base seen =
      let name k = if k == (1 :: Int) then base else base <> "_" <> tshow k
          n = head' [k | k <- [M.findWithDefault 1 base seen ..], not (M.member (name k) seen), ok (name k)]
       in (name n, M.insert base (n + 1) (M.insert (name n) 2 seen))
    head' xs = case xs of
      (x : _) -> x
      [] -> 0

-- | A translated title as an ODM @Description@; untranslated titles are
-- carried by @Name@ alone.
description :: Maybe Lang -> Maybe I18n -> [Node]
description defLang = \case
  Just t | not (null (i18nLangs t)) -> [el "Description" [] (langNodes defLang "TranslatedText" [] t)]
  _ -> []

-- | An ItemGroupDef to write: OID, name, title, repeating, and its items.
data Run = Run Text Text (Maybe I18n) Bool [Item]

-- | Consecutive loose items form one group named like the form (read back
-- as the form's own items); each section or repeat is its own group.
groupRuns :: Text -> Text -> [Item] -> Conv [Run]
groupRuns fid fname items0 = concat <$> mapM inline items0 >>= go (1 :: Int)
  where
    -- read back, a group named like its form is the form's own items
    inline = \case
      ItemGroup g | null [() | ItemQuestion _ <- flatten [ItemGroup g]] && null [() | ItemNote {} <- flatten [ItemGroup g]] -> do
        warn (groupId g) "empty group dropped (an ODM item group needs items)"
        pure []
      ItemGroup g | groupName g == fname, groupKind g /= Repeat -> do
        warn (groupId g) "section named like its page merged into the page"
        pure (flatten [ItemGroup g])
      i -> pure [i]
    go _ [] = pure []
    go n (ItemGroup g : rest) = do
      when (isJust (groupRelevant g)) $ warn (groupId g) "section logic copied onto its questions"
      let nested = [x | ItemGroup x <- groupItems g]
      unless (null nested) $ warn (groupId g) "nested groups flattened into their section"
      when (groupKind g == Page) $ warn (groupId g) "nested page written as a section"
      let kids = flatten (map (pushRelevant (groupRelevant g)) (groupItems g))
          name = groupName g
      (Run (fid <> "." <> groupId g) name (groupTitle g) (groupKind g == Repeat) kids :) <$> go n rest
    go n is =
      let (loose, rest) = break isGroup is
       in (Run (fid <> ".items" <> tshow n) fname Nothing False loose :) <$> go (n + 1) rest
    isGroup = \case
      ItemGroup _ -> True
      _ -> False
    flatten = concatMap $ \case
      ItemGroup g -> flatten (map (pushRelevant (groupRelevant g)) (groupItems g))
      i -> [i]

-- | An item group's Name: its title, or its id when the title is blank
-- (a blank name reads back as the form's own items).
groupName :: Group -> Text
groupName g = fromMaybe (groupId g) (groupTitle g >>= nonBlank . i18nDefault)

pushRelevant :: Maybe Expr -> Item -> Item
pushRelevant Nothing i = i
pushRelevant (Just e) i = case i of
  ItemQuestion q -> ItemQuestion q {qRelevant = Just (maybe e (EBin And e) (qRelevant q))}
  ItemNote n t r -> ItemNote n t (Just (maybe e (EBin And e) r))
  ItemGroup g -> ItemGroup g {groupRelevant = Just (maybe e (EBin And e) (groupRelevant g))}
  _ -> i

itemGroup :: Maybe Lang -> Run -> Conv Defs
itemGroup defLang (Run oid name title repeating kids) = do
  outs <- concat <$> mapM (itemOuts defLang) kids
  let grp = el "ItemGroupDef" [("OID", oid), ("Name", name), ("Repeating", if repeating then "Yes" else "No")] (description defLang title ++ map oRef outs)
  if null outs
    then mempty <$ when (isJust title || repeating) (warn oid "empty group dropped (an ODM item group needs items)")
    else pure (Defs [] [grp] [] [] [] <> mconcat (map oDefs outs))

itemOuts :: Maybe Lang -> Item -> Conv [Out]
itemOuts defLang = \case
  ItemRaw (Format f) _ -> [] <$ warn "odm" ("raw " <> f <> " content dropped")
  ItemGroup g -> concat <$> mapM (itemOuts defLang) (groupItems g)
  ItemNote i t rel -> do
    b <- printLogic redcap i rel
    pure [itemDef defLang i t "text" [("FieldType", "descriptive")] b False [] Nothing [] Nothing]
  ItemQuestion q -> questionOuts defLang q

questionOuts :: Maybe Lang -> Question -> Conv [Out]
questionOuts defLang q = do
  b <- printLogic redcap (qId q) (qRelevant q)
  when (isJust (qValidation q >>= valExpr) && null (exprChecks q)) $ warn (qId q) "validation expression dropped"
  when (isJust (qDefault q)) $ warn (qId q) "default value dropped"
  let i = qId q
      annotation = [v | Just (String v) <- [M.lookup "redcap:annotation" (qAttrs q)]]
      common =
        [("FieldNote", i18nDefault h) | Just h <- [qHint q]]
          ++ [("RequiredField", "y") | qRequired q]
          ++ [("Identifier", v) | Just (String v) <- [M.lookup "redcap:identifier" (qAttrs q)]]
          ++ [("CustomAlignment", v) | Just (String v) <- [M.lookup "redcap:alignment" (qAttrs q)]]
      withAnnot extra = [("FieldAnnotation", t) | let t = T.unwords (extra ++ annotation), not (T.null t)]
      def dt ft extra cl = do
        when (maybe False (not . null . i18nLangs) (qHint q)) $ warn i "hint translations dropped"
        pure [itemDef defLang i (qLabel q) dt ([("FieldType", ft)] ++ extra ++ common ++ withAnnot []) b (qRequired q) (rangeOf q) (qValidation q >>= valMessage) [] cl]
      simple dt ft extra = def dt ft extra Nothing
      codeList cs = Just (i <> ".choices", [(choiceValue c, choiceLabel c) | c <- cs])
      dataTypeFor cs = if all (isCode . choiceValue) cs then "integer" else "text"
  case qType q of
    QText ShortText -> simple "text" "text" []
    QText LongText -> simple "text" "textarea" []
    QText Email -> simple "text" "text" [("TextValidationType", "email")]
    QText Phone -> simple "text" "text" [("TextValidationType", "phone")]
    QText Url -> simple "URI" "text" []
    QNumber Integer -> simple "integer" "text" [("TextValidationType", "int")]
    QNumber Decimal -> simple "float" "text" [("TextValidationType", "float")]
    QDate Date -> simple "date" "text" [("TextValidationType", "date_ymd")]
    QDate Time -> simple "time" "text" [("TextValidationType", "time")]
    QDate DateTime -> simple "datetime" "text" [("TextValidationType", "datetime_ymd")]
    QBoolean -> def "boolean" "yesno" [] (Just (i <> ".choices", [("1", plain "Yes"), ("0", plain "No")]))
    QFile -> simple "text" "file" []
    QGeo -> simple "text" "text" [] <* warn i "geopoint written as a text field"
    QOther t -> simple "text" "text" [] <* warn i ("type " <> t <> " written as a text field")
    QHidden -> pure [itemDef defLang i (qLabel q) "text" ([("FieldType", "text")] ++ common ++ withAnnot ["@HIDDEN" | not (any ("@HIDDEN" `T.isInfixOf`) annotation)]) b (qRequired q) [] Nothing [] Nothing]
    QCalculate e -> do
      c <- printLogic redcap i (Just e)
      simple "float" "calc" [("Calculation", fromMaybe "" c)]
    QScale lo hi l h ->
      pure
        [ itemDef defLang i (qLabel q) "integer" ([("FieldType", "slider"), ("SliderLabels", T.intercalate " | " [maybe "" i18nDefault l, "", maybe "" i18nDefault h])] ++ common ++ withAnnot []) b (qRequired q) [("GE", fromIntegral lo), ("LE", fromIntegral hi)] Nothing [] Nothing
        ]
          <* when (any (maybe False (not . null . i18nLangs)) [l, h]) (warn i "slider label translations dropped")
    QChoice Single style cs -> do
      when (any choiceOther cs) $ warn i "\"other, specify\" text box dropped"
      when (style == Buttons) $ warn i "button display style dropped"
      def (dataTypeFor cs) (if style == Dropdown then "select" else "radio") [] (codeList cs)
    QChoice Multiple _ cs -> do
      when (any choiceOther cs) $ warn i "\"other, specify\" text box dropped"
      checkboxes i (qLabel q) cs b
    QMatrix sel rows cols -> do
      rowOuts <-
        concat
          <$> forM
            (zip [0 :: Int ..] rows)
            ( \(n, r) -> do
                let rid = i <> "_" <> choiceValue r
                    header = [("SectionHeader", i18nDefault (qLabel q)) | n == 0, not (T.null (i18nDefault (qLabel q)))]
                    extra = header ++ [("MatrixGroupName", i)]
                case sel of
                  Single -> pure [itemDef defLang rid (choiceLabel r) (dataTypeFor cols) ([("FieldType", "radio")] ++ extra ++ common) b (qRequired q) [] Nothing [] (Just (rid <> ".choices", [(choiceValue c, choiceLabel c) | c <- cols]))]
                  Multiple -> checkboxesWith extra rid (choiceLabel r) cols b
            )
      pure rowOuts
    QRanking _ -> do
      expanded <- expandQuestion q
      concat <$> mapM (questionOuts defLang) expanded
  where
    rangeOf x = case qValidation x of
      Just v -> [("GE", n) | Just n <- [valMin v]] ++ [("LE", n) | Just n <- [valMax v]] ++ fromMaybe [] (exprChecks x)
      _ -> []
    checkboxes = checkboxesWith []
    -- REDCap's layout: one boolean item per option, @var___code@.
    checkboxesWith extra var label cs b =
      let none = [choiceValue c | c <- cs, choiceExclusive c]
          annotation = T.unwords (["@NONEOFTHEABOVE='" <> T.intercalate "," none <> "'" | not (null none)] ++ [v | Just (String v) <- [M.lookup "redcap:annotation" (qAttrs q)]])
          choicesAttr = T.intercalate " | " [choiceValue c <> ", " <> i18nDefault (choiceLabel c) | c <- cs]
       in pure
            [ withDescription defLang (choiceLabel c) $ itemDef
                defLang
                (var <> "___" <> choiceValue c)
                label
                "boolean"
                ([("Variable", var), ("FieldType", "checkbox")] ++ extra ++ [("FieldNote", i18nDefault h) | Just h <- [qHint q]] ++ [("RequiredField", "y") | qRequired q] ++ [("FieldAnnotation", annotation) | not (T.null annotation)])
                b
                (qRequired q)
                []
                Nothing
                [("CheckboxChoices", choicesAttr)]
                (Just (var <> "___" <> choiceValue c <> ".choices", [("1", plain "Checked"), ("0", plain "Unchecked")]))
            | c <- cs
            ]

-- | A validation rule that is only bounds on the answer, as range checks.
-- A strict bound on a whole number is written as the next value in, as
-- the reader reads it; on any other answer the reader cannot keep it
-- exact, so such a rule is not written this way.
exprChecks :: Question -> Maybe [(Text, Double)]
exprChecks q = do
  cs <- qValidation q >>= valExpr >>= rangeRule
  if qType q == QNumber Integer
    then Just (map whole cs)
    else if any ((`elem` ["GT", "LT"]) . fst) cs then Nothing else Just cs
  where
    whole (c, v) = case c of
      "GT" -> ("GE", fromIntegral (floor v + 1 :: Integer))
      "LT" -> ("LE", fromIntegral (ceiling v - 1 :: Integer))
      _ -> (c, v)

isScale :: QType -> Bool
isScale = \case
  QScale {} -> True
  _ -> False

-- | ODM's range check comparators as logic, and back: a rule that is only
-- bounds on the answer (@. > 3 and . <= 9@) is written as range checks.
comparatorOp :: Text -> BinOp
comparatorOp = \case
  "GT" -> Gt
  "LT" -> Lt
  "LE" -> Le
  _ -> Ge

rangeRule :: Expr -> Maybe [(Text, Double)]
rangeRule = \case
  EBin And a b -> (++) <$> rangeRule a <*> rangeRule b
  EBin op (ERef ".") (ELit (LNum v)) -> (\c -> [(c, v)]) <$> lookup op [(Gt, "GT"), (Lt, "LT"), (Ge, "GE"), (Le, "LE")]
  _ -> Nothing

-- | An option's own label as the ItemDef's @Description@ (exact, and
-- translatable, unlike REDCap's @CheckboxChoices@ list).
withDescription :: Maybe Lang -> I18n -> Out -> Out
withDescription defLang l o = o {oDefs = (oDefs o) {dItems = map add (dItems (oDefs o))}}
  where
    add (El "ItemDef" as kids) = El "ItemDef" as (el "Description" [] (texts defLang l) : kids)
    add n = n

-- | @TranslatedText@s. When the default language is known every default
-- text is tagged with it, so the reader can tell it from the first one.
texts :: Maybe Lang -> I18n -> [Node]
texts defLang t = case langNodes defLang "TranslatedText" [] t of
  (El n [] kids : rest) | Just l <- defLang -> El n [("xml:lang", l)] kids : rest
  ns -> ns

-- | One ItemDef with its ItemRef, code list and exception condition.
-- @redcap@ attributes are given without their prefix; @Variable@ defaults
-- to the item name.
itemDef :: Maybe Lang -> Text -> I18n -> Text -> [(Text, Text)] -> Maybe Text -> Bool -> [(Text, Double)] -> Maybe I18n -> [(Text, Text)] -> Maybe (Text, [(Text, I18n)]) -> Out
itemDef defLang oid label dtype rc branching mandatory range message clAttrs cl =
  let rcAttrs = (if isJust (lookup "Variable" rc) then [] else [("Variable", oid)]) ++ rc ++ [("BranchingLogic", t) | Just t <- [branching]]
      condition = do
        t <- branching
        e <- either (const Nothing) Just (parseExpr redcap t)
        neg <- either (const Nothing) Just (printExpr redcap (fromMaybe (ENot e) (pushNot e)))
        pure (el "ConditionDef" [("OID", "C." <> oid), ("Name", "Skip " <> oid)] [leaf "FormalExpression" [("Context", "REDCap")] neg])
      ref = el "ItemRef" ([("ItemOID", oid), ("Mandatory", if mandatory then "Yes" else "No")] ++ [("CollectionExceptionConditionOID", "C." <> oid) | isJust condition]) []
      checks = [el "RangeCheck" [("Comparator", c), ("SoftHard", "Hard")] (leaf "CheckValue" [] (fmtNum v) : errorMessage) | (c, v) <- range]
      errorMessage = [el "ErrorMessage" [] (texts defLang m) | Just m <- [message]]
      def =
        el "ItemDef" ([("OID", oid), ("Name", oid), ("DataType", dtype)] ++ [("redcap:" <> k, v) | (k, v) <- rcAttrs]) $
          [el "Question" [] (texts defLang label)]
            ++ checks
            ++ [el "CodeListRef" [("CodeListOID", clOid)] [] | Just (clOid, _) <- [cl]]
      codeList =
        [ el "CodeList" ([("OID", clOid), ("Name", oid), ("DataType", if dtype == "boolean" then "boolean" else if all (isCode . fst) cs then "integer" else "text")] ++ [("redcap:" <> k, v) | (k, v) <- clAttrs])
            [el "CodeListItem" [("CodedValue", c)] [el "Decode" [] (texts defLang l)] | (c, l) <- cs]
        | Just (clOid, cs) <- [cl]
        ]
   in Out ref (Defs [] [] [def] codeList (maybe [] pure condition))
