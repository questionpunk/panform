-- | Formbricks survey JSON (@packages/types/surveys@ zod schema). A survey
-- is @blocks[]@ (screens) of @elements[]@, or in the older v1 model a flat
-- @questions[]@ with one question per screen. Strings are
-- @{"default": ..., "de": ...}@ maps; logic is structured condition groups
-- whose only routing action is a jump, so show-if conditions travel as
-- jump chains (see "Panform.Jump").
module Panform.Format.Formbricks (readFormbricks, writeFormbricks) where

import Control.Monad (forM, forM_, mfilter, unless, when)
import Data.Aeson (Result (..), Value (..), eitherDecode, fromJSON, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isDigit, toLower)
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bifunctor (first)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Panform.AST
import Panform.Conv
import Panform.Design (readDesign, writeDesign)
import Panform.Expr (pushNot)
import Panform.Expr.Simplify (simplify)
import Panform.Json
import Panform.Jump

------------------------------------------------------------------------------
-- Reading

readFormbricks :: BL.ByteString -> Conv Form
readFormbricks bs = do
  raw <- eitherDecode bs `orFail` "Formbricks JSON"
  -- a Management API response wraps the survey in "data"
  let v = case key "data" raw of
        Just d@(Object _) | isJust (key "questions" d) || isJust (key "blocks" d) -> d
        _ -> raw
  let hidden =
        [ fid
        | Just hf <- [key "hiddenFields" v]
        , keyBool "enabled" hf /= Just False
        , Just fid <- map asText (keyArr "fieldIds" hf)
        ]
      vars = keyArr "variables" v
      varNames = M.fromList [(i, n) | x <- vars, Just i <- [keyText "id" x], Just n <- [keyText "name" x]]
      -- conditions in the form the writer would give them back
      norm = settle (Ctx hidden (M.fromList [(n, i) | (i, n) <- M.toList varNames]))
      endings = mapMaybe (keyText "id") (keyArr "endings" v)
      blocks = keyArr "blocks" v
      hiddenItems = [ItemQuestion (question h h QHidden) | h <- hidden]
      welcome = key "welcomeCard" v
  (items, routing) <-
    if null blocks
      then readQuestionsModel norm varNames endings (keyArr "questions" v)
      else readBlocksModel norm varNames endings blocks
  let meta =
        emptyMeta
          { metaId = keyText "id" v
          , metaTitle = (welcome >>= key "headline" >>= loc) `orElse` (plain <$> (keyText "name" v >>= realName))
          , metaDescription = welcome >>= key "subheader" >>= loc
          , metaLanguages = readLanguages v
          , metaAttrs =
              readDesign "formbricks" v <> M.fromList
                ( [("formbricks:variables", toJSON vars) | not (null vars)]
                    ++ [("formbricks:endings", e) | Just e@(Array a) <- [key "endings" v], not (V.null a), not (defaultEnding (V.toList a))]
                    ++ [("formbricks:routing", r) | Just r <- [routing]]
                )
          }
  pure (Form meta (hiddenItems ++ items))
  where
    orElse a b = maybe b Just a
    -- the writer's placeholder for a survey without a title
    realName n = if n == "Untitled" then Nothing else Just n

-- | The bare ending card the writer adds when a form has none.
defaultEnding :: [Value] -> Bool
defaultEnding = \case
  [Object o]
    | KM.size o == 2
    , Just (String "endScreen") <- KM.lookup "type" o
    , Just (String i) <- KM.lookup "id" o ->
        maybe False (T.all isDigit) (T.stripPrefix "end" i)
  _ -> False

-- | The default language first, then the others.
readLanguages :: Value -> [Lang]
readLanguages v = [c | (True, c) <- ls] ++ [c | (False, c) <- ls]
  where
    ls =
      [ (keyBool "default" l == Just True, c)
      | l <- keyArr "languages" v
      , Just c <- [(key "language" l >>= keyText "code") `orElse` keyText "language" l]
      ]
    orElse a b = maybe b Just a

-- | A Formbricks i18n string.
loc :: Value -> Maybe I18n
loc = \case
  String t -> Just (plain t)
  Object o -> do
    let m = M.fromList [(K.toText k, t) | (k, String t) <- KM.toList o]
    d <- M.lookup "default" m
    Just (I18n d (M.delete "default" m))
  _ -> Nothing

readBlocksModel :: (Expr -> Expr) -> M.Map Text Text -> [Text] -> [Value] -> Conv ([Item], Maybe Value)
readBlocksModel norm varNames endings blocks = do
  let ids = [keyTextOr ("block" <> T.pack (show n)) "id" b | (n, b) <- zip [1 :: Int ..] blocks]
      index = M.fromList (zip ids [0 ..]) `M.union` M.fromList [(e, length blocks) | e <- endings]
  units <- forM (zip ids blocks) $ \(bid, b) -> readLogic varNames index bid (keyArr "logic" b) (keyText "logicFallback" b)
  (rels, approx) <- first (map (fmap norm)) <$> jumpsToRelevanceApprox (ids !!) units
  routing <- keptRouting ids blocks rels approx
  fmap (,routing) . forM (zip3 ids blocks rels) $ \(bid, b, rel) -> do
    kids <- mapM readElement (keyArr "elements" b)
    let name = keyText "name" b
    pure . ItemGroup $
      Group
        { groupId = bid
        , groupKind = Page
        , groupTitle = plain <$> (if name == Just bid then Nothing else name)
        , groupRelevant = rel
        , groupItems = kids
        , groupAttrs = M.fromList [("formbricks:" <> k, x) | k <- ["buttonLabel", "backButtonLabel"], Just x <- [key k b]]
        }

-- | The v1 model: every question is its own screen with its own logic.
readQuestionsModel :: (Expr -> Expr) -> M.Map Text Text -> [Text] -> [Value] -> Conv ([Item], Maybe Value)
readQuestionsModel norm varNames endings qs = do
  let ids = [keyTextOr ("q" <> T.pack (show n)) "id" q | (n, q) <- zip [1 :: Int ..] qs]
      index = M.fromList (zip ids [0 ..]) `M.union` M.fromList [(e, length qs) | e <- endings]
  units <- forM (zip ids qs) $ \(qid, q) -> readLogic varNames index qid (keyArr "logic" q) (keyText "logicFallback" q)
  (rels, approx) <- first (map (fmap norm)) <$> jumpsToRelevanceApprox (ids !!) units
  routing <- keptRouting ids qs rels approx
  fmap (,routing) . forM (zip qs rels) $ \(q, rel) -> do
    it <- readElement (dropKeys ["logic", "logicFallback", "buttonLabel", "backButtonLabel"] q)
    pure $ case it of
      ItemQuestion x -> ItemQuestion x {qRelevant = rel}
      ItemNote i t _ -> ItemNote i t rel
      other -> other

-- | When some screen's condition was too large to keep, the routing as
-- written, with the conditions read alongside it: the writer gives the
-- routing back unchanged while the conditions are unchanged.
keptRouting :: [Text] -> [Value] -> [Maybe Expr] -> [Int] -> Conv (Maybe Value)
keptRouting ids screens rels approx
  | null approx = pure Nothing
  | otherwise = do
      warn "formbricks" "jump logic kept as written for formbricks; other formats show the screens named above whenever they are reached"
      pure . Just $
        object
          [ "screens" .= [object (["id" .= i] ++ ["logic" .= l | Just l <- [key "logic" b]] ++ ["logicFallback" .= f | Just f <- [key "logicFallback" b]]) | (i, b) <- zip ids screens]
          , "relevance" .= rels
          ]

-- | The kept routing, when the screens and their conditions are still the
-- ones it was read with: each screen's logic and fallback.
routingFor :: Value -> [(Text, Maybe Expr)] -> Maybe [(Maybe Value, Maybe Value)]
routingFor r screens = do
  ids <- mapM (keyText "id") (keyArr "screens" r)
  rels <- case fromJSON <$> key "relevance" r of
    Just (Success x) -> Just x
    _ -> Nothing
  if ids == map fst screens && rels == map snd screens
    then Just [(key "logic" x, key "logicFallback" x) | x <- keyArr "screens" r]
    else Nothing

dropKeys :: [Text] -> Value -> Value
dropKeys ks = \case
  Object o -> Object (foldr (KM.delete . K.fromText) o ks)
  x -> x

-- | One screen's logic entries as jump rules plus fallback.
readLogic :: M.Map Text Text -> M.Map Text Int -> Text -> [Value] -> Maybe Text -> Conv ([Rule], Maybe Int)
readLogic varNames index at entries fallback = do
  rules <- fmap catMaybes . forM entries $ \l -> do
    let actions = keyArr "actions" l
        jumps = [t | a <- actions, keyText "objective" a `elem` [Just "jumpToBlock", Just "jumpToQuestion"], Just t <- [keyText "target" a]]
    forM_' actions $ \a -> case keyText "objective" a of
      Just o | o `elem` ["jumpToBlock", "jumpToQuestion"] -> pure ()
      Just o -> warn at (o <> " action dropped")
      Nothing -> warn at "logic action without an objective dropped"
    case (key "conditions" l, jumps) of
      (_, []) -> pure Nothing
      (Nothing, _) -> Nothing <$ warn at "logic entry without conditions dropped"
      (Just c, t : _) -> case (readGroup varNames c, M.lookup t index) of
        (Left why, _) -> Nothing <$ warn at ("logic dropped, " <> why)
        (_, Nothing) -> Nothing <$ warn at ("jump to unknown target " <> t <> " dropped")
        (Right e, Just k) -> pure (Just (Rule (fromMaybe (ELit (LBool True)) e) k))
  fb <- case fallback of
    Nothing -> pure Nothing
    Just t -> case M.lookup t index of
      Just k -> pure (Just k)
      Nothing -> Nothing <$ warn at ("fallback to unknown target " <> t <> " dropped")
  pure (rules, fb)
  where
    forM_' xs f = mapM_ f xs

-- | A condition group; 'Nothing' for an empty (always true) group.
readGroup :: M.Map Text Text -> Value -> Either Text (Maybe Expr)
readGroup varNames g = case keyArr "conditions" g of
  [] | isJust (key "leftOperand" g) -> Just <$> readSingle varNames g
  cs -> do
    es <- catMaybes <$> mapM (\c -> if isJust (key "connector" c) then readGroup varNames c else Just <$> readSingle varNames c) cs
    pure $ if keyText "connector" g == Just "or" then disj es else conj es

readSingle :: M.Map Text Text -> Value -> Either Text Expr
readSingle varNames c = do
  l <- maybe (Left "condition without a left operand") operand (key "leftOperand" c)
  q <- case l of
    ERef q -> Right q
    _ -> Left "left operand is not a field"
  op <- maybe (Left "condition without an operator") Right (keyText "operator" c)
  let right = key "rightOperand" c
      statics = case right >>= key "value" of
        Just (Array a) -> mapMaybe asText (V.toList a)
        Just x -> maybe [] pure (asText x)
        Nothing -> []
  r <- case right of
    Just o | Just (Array _) <- key "value" o -> Right Nothing
    _ -> traverse operand right
  let one f = case r of
        Just x -> Right (f x)
        Nothing -> Left (op <> " without a value")
      many f comb = maybe (Left (op <> " without values")) Right (comb (map f statics))
  case op of
    "equals" -> one (EBin Eq (ERef q))
    "doesNotEqual" -> one (EBin Neq (ERef q))
    "isGreaterThan" -> one (EBin Gt (ERef q))
    "isLessThan" -> one (EBin Lt (ERef q))
    "isGreaterThanOrEqual" -> one (EBin Ge (ERef q))
    "isLessThanOrEqual" -> one (EBin Le (ERef q))
    "includesOneOf" -> many (ESelected q) disj
    "includesAllOf" -> many (ESelected q) conj
    "doesNotIncludeOneOf" -> many (ENot . ESelected q) conj
    "doesNotIncludeAllOf" -> many (ENot . ESelected q) disj
    "equalsOneOf" -> many (EBin Eq (ERef q) . ELit . LStr) disj
    "isAnyOf" -> many (EBin Eq (ERef q) . ELit . LStr) disj
    "isSubmitted" -> Right (EAnswered q)
    "isSkipped" -> Right (ENot (EAnswered q))
    _ -> Right (ECall op (ERef q : maybe [] pure r))
  where
    operand o = case keyText "type" o of
      Just "static" -> case key "value" o of
        Just (Number n) -> Right (ELit (LNum (toRealFloat n)))
        Just (String s) -> Right (ELit (LStr s))
        Just (Bool b) -> Right (ELit (LBool b))
        _ -> Left "unsupported static operand"
      Just "variable" -> Right (ERef (M.findWithDefault (keyTextOr "" "value" o) (keyTextOr "" "value" o) varNames))
      Just "reserved" -> Right (ERef ("reserved:" <> keyTextOr "" "value" o))
      Just _ -> maybe (Left "operand without a value") (Right . ERef) (keyText "value" o)
      Nothing -> Left "operand without a type"

-- | Element keys the reader maps into the IR; the rest are kept as
-- @formbricks:<key>@ attributes.
consumed :: [Text]
consumed =
  [ "id", "type", "headline", "subheader", "required", "choices", "rows", "columns", "lowerLabel", "upperLabel"
  , "range", "inputType", "longAnswer", "charLimit", "validation", "allowMulti", "displayType"
  ]

readElement :: Value -> Conv Item
readElement e = case ty of
  "cta" -> do
    let extra = [k | k <- attrKeys, k `notElem` ["buttonExternal", "isDraft"]] ++ ["subheader" | isJust (key "subheader" e)]
    unless (null extra) $ warn qid ("call-to-action fields dropped: " <> T.intercalate ", " extra)
    pure (ItemNote qid label Nothing)
  _ -> do
    (qt, keepType) <- readType
    val <- readValidation
    pure . ItemQuestion $
      Question
        { qId = qid
        , qLabel = label
        , qHint = key "subheader" e >>= loc
        , qType = qt
        , qRequired = keyBool "required" e == Just True
        , qRelevant = Nothing
        , qValidation = val
        , qDefault = Nothing
        , qAttrs =
            M.fromList $
              [("formbricks:" <> k, x) | k <- attrKeys, Just x <- [key k e]]
                ++ [("formbricks:type", String ty) | keepType]
                ++ [("formbricks:validation", x) | Just x <- [key "validation" e], not (simpleRules x)]
        }
  where
    qid = keyTextOr "" "id" e
    ty = keyTextOr "" "type" e
    label = fromMaybe (plain qid) (key "headline" e >>= loc)
    attrKeys = case e of
      Object o -> [K.toText k | k <- KM.keys o, K.toText k `notElem` consumed]
      _ -> []
    choices = [readChoice c | c <- keyArr "choices" e]
    rng = maybe 5 round (keyNum "range" e)
    lower = key "lowerLabel" e >>= loc
    upper = key "upperLabel" e >>= loc
    readType = case ty of
      "openText" -> pure . (,False) $ case keyTextOr "text" "inputType" e of
        "email" -> QText Email
        "url" -> QText Url
        "phone" -> QText Phone
        "number" -> QNumber Decimal
        _ | keyBool "longAnswer" e == Just True -> QText LongText
        _ -> QText ShortText
      "multipleChoiceSingle" -> pure (QChoice Single (style Radio) choices, False)
      "multipleChoiceMulti" -> pure (QChoice Multiple (style Radio) choices, False)
      "pictureSelection" ->
        pure (QChoice (if keyBool "allowMulti" e == Just True then Multiple else Single) Buttons [choice i u | c <- keyArr "choices" e, let i = keyTextOr "" "id" c, let u = keyTextOr "" "imageUrl" c], True)
      "nps" -> pure (QScale 0 10 lower upper, False)
      "rating" -> pure (QScale 1 rng lower upper, False)
      "csat" -> pure (QScale 1 rng lower upper, True)
      "ces" -> pure (QScale 1 rng lower upper, True)
      "consent" -> pure (QBoolean, False)
      "date" -> pure (QDate Date, False)
      "fileUpload" -> pure (QFile, False)
      "matrix" -> pure (QMatrix Single (map readChoice (keyArr "rows" e)) (map readChoice (keyArr "columns" e)), False)
      "ranking" -> pure (QRanking choices, False)
      t | t `elem` ["cal", "address", "contactInfo"] -> pure (QOther t, False)
      t -> (QOther t, False) <$ warn qid ("unsupported type " <> t <> " kept by name")
    style d = if keyText "displayType" e == Just "dropdown" then Dropdown else d
    readValidation = do
      forM_ (key "validation" e) $ \v ->
        unless (simpleRules v) $ warn qid "validation rules kept only for formbricks"
      -- Bounds joined by "or" are not a range.
      let rules = maybe [] (keyArr "rules") (mfilter conjunctive (key "validation" e))
          rule t = [n | r <- rules, keyText "type" r == Just t, Just p <- [key "params" r], Just n <- [keyNum "min" p `orElse` keyNum "max" p]]
          charLimit = key "charLimit" e
          fromChars = case charLimit of
            Just c | keyBool "enabled" c == Just True -> (keyNum "min" c, keyNum "max" c)
            _ -> (Nothing, Nothing)
          lo = firstJust [fst fromChars, headMay (rule "minValue"), headMay (rule "minLength")]
          hi = firstJust [snd fromChars, headMay (rule "maxValue"), headMay (rule "maxLength")]
      pure $ if isJust lo || isJust hi then Just (Validation lo hi Nothing Nothing) else Nothing
    orElse a b = maybe b Just a
    firstJust = foldr orElse Nothing
    headMay = \case
      (x : _) -> Just x
      [] -> Nothing

-- | Validation made only of min/max rules, each at most once and all
-- required together, maps onto the IR; anything else is kept whole as an
-- attribute.
simpleRules :: Value -> Bool
simpleRules v = conjunctive v && all simple rs && length types == length (nub types)
  where
    rs = keyArr "rules" v
    types = map (keyText "type") rs
    simple r = keyText "type" r `elem` map Just ["minValue", "maxValue", "minLength", "maxLength"]

-- | Every rule must hold: one rule, or rules joined by "and".
conjunctive :: Value -> Bool
conjunctive v = length (keyArr "rules" v) <= 1 || keyText "logic" v == Just "and"

readChoice :: Value -> Choice
readChoice c =
  let i = keyTextOr "" "id" c
      l = fromMaybe (plain i) (key "label" c >>= loc)
   in Choice i l (i == "other") (i == "none")

------------------------------------------------------------------------------
-- Writing

-- | What the writer needs to resolve references in conditions.
data Ctx = Ctx
  { ctxHidden :: [Text]
  , ctxVars :: M.Map Text Text
  -- ^ Variable name to id.
  }

writeFormbricks :: Form -> Conv BL.ByteString
writeFormbricks (Form meta items) = do
  let hidden = [qId q | ItemQuestion q <- allItems items, qType q == QHidden]
      vars = case M.lookup "formbricks:variables" (metaAttrs meta) of
        Just (Array a) -> V.toList a
        _ -> []
      ctx = Ctx hidden (M.fromList [(n, i) | x <- vars, Just i <- [keyText "id" x], Just n <- [keyText "name" x]])
  sequence_ [warn (qId q) "hidden field label dropped" | ItemQuestion q <- allItems items, qType q == QHidden, qLabel q /= plain (qId q)]
  let visible = filter (not . isHidden) items
  -- A form without pages is written in the v1 model, one question per
  -- screen, which is how the reader reads it back.
  flatItems <- concat <$> mapM flattenItem visible
  let shown = filter emits flatItems
      v1 =
        not (null visible)
          && null [() | ItemGroup _ <- visible]
          && S.size (S.fromList (map itemId shown)) == length shown
  screens <-
    if v1
      then do
        mapM_ writeElement (filter (not . emits) flatItems)
        pure [Screen (itemId i) Nothing (itemRel i) [strip i] M.empty | i <- shown]
      else paginate visible >>= fmap concat . mapM toScreens >>= uniqueScreens
  let freeEnd = headOr "end" [e | e <- "end" : ["end" <> T.pack (show n) | n <- [2 :: Int ..]], e `notElem` map scId screens]
      endings = case M.lookup "formbricks:endings" (metaAttrs meta) of
        Just e@(Array a) | not (V.null a) -> e
        _ -> toJSON [object ["id" .= freeEnd, "type" .= ("endScreen" :: Text)]]
      endId = case endings of
        Array a | (x : _) <- V.toList a -> keyTextOr freeEnd "id" x
        _ -> freeEnd
  let kept = M.lookup "formbricks:routing" (metaAttrs meta) >>= \r -> routingFor r [(scId s, scRel s) | s <- screens]
  when (isJust (M.lookup "formbricks:routing" (metaAttrs meta)) && isNothing kept) $
    warn "formbricks" "jump logic kept from the source no longer matches the screens; written from the conditions"
  rels <- forM (zip [0 :: Int ..] screens) $ \(n, s) -> if isJust kept then pure Nothing else case scRel s of
    Nothing -> pure Nothing
    Just e
      | n == 0 -> Nothing <$ warn (scId s) "condition on the first screen dropped, nothing jumps to it"
      | otherwise -> case toGroup ctx "x" e of
          Left why -> Nothing <$ warn (scId s) ("logic dropped, formbricks cannot express " <> why)
          Right _ -> pure (Just (settle ctx e))
  let jumps = relevanceToJumps rels
      nScreens = length screens
      target k = if k >= nScreens then endId else maybe endId scId (lookupIdx k screens)
  blocks <- forM (zip3 [0 :: Int ..] screens jumps) $ \(n, s, (rules, fb)) -> do
    els <- concat <$> mapM writeElement (scItems s)
    logic <- forM (zip [1 :: Int ..] rules) $ \(r, Rule e k) -> do
      let lid = "l" <> T.pack (show n) <> "r" <> T.pack (show r)
      cond <- either (\why -> failWith ("formbricks: " <> why)) pure (toGroup ctx lid e)
      pure $ object ["id" .= lid, "conditions" .= cond, "actions" .= [object ["id" .= (lid <> "a"), "objective" .= (if v1 then "jumpToQuestion" else "jumpToBlock" :: Text), "target" .= target k]]]
    let rawAt = case kept of
          Just rs | (lg, f) : _ <- drop n rs -> Just (lg, f)
          _ -> Nothing
        logicFields = case rawAt of
          Just (lg, f) -> [("logic", x) | Just x <- [lg]] ++ [("logicFallback", x) | Just x <- [f]]
          Nothing -> [("logic", toJSON logic) | not (null logic)] ++ [("logicFallback", String (target f)) | Just f <- [fb]]
    pure $ if v1
      then case els of
        [Object o] -> Object (foldr (\(k, x) -> KM.insert k x) o logicFields)
        _ -> toJSON els
      else obj $
      [ "id" .=? String (scId s)
      , "name" .=? String (maybe (scId s) i18nDefault (scTitle s))
      , "elements" .=? toJSON els
      ]
          ++ map Just logicFields
          ++ [Just (K.fromText (T.drop 11 k), x) | (k, x) <- M.toList (scAttrs s), "formbricks:" `T.isPrefixOf` k]
  let langs = metaLanguages meta
  let welcome =
        obj
          [ "enabled" .=? Bool (isJust (metaDescription meta))
          , ("headline" .=?) . i18nJSON =<< metaTitle meta
          , ("subheader" .=?) . i18nJSON =<< metaDescription meta
          ]
      computed =
        [ ("name", String (maybe "Untitled" i18nDefault (metaTitle meta)))
        , ("type", "link")
        , ("welcomeCard", welcome)
        , (if v1 then "questions" else "blocks", toJSON blocks)
        , ("endings", endings)
        , ("hiddenFields", object ["enabled" .= not (null hidden), "fieldIds" .= hidden])
        , ("variables", toJSON vars)
        ]
          ++ [("id", String i) | Just i <- [metaId meta]]
          ++ [("languages", toJSON [object ["language" .= object ["code" .= l], "default" .= (i == (0 :: Int)), "enabled" .= True] | (i, l) <- zip [0 ..] langs]) | not (null langs)]
      extra = [(K.fromText (T.drop 11 k), x) | (k, x) <- M.toList (metaAttrs meta), "formbricks:" `T.isPrefixOf` k, T.drop 11 k `notElem` ("design" : map fst computed)]
  pure . encodePretty $ object ([(K.fromText k, x) | (k, x) <- computed] ++ extra ++ writeDesign "formbricks" meta)
  where
    isHidden = \case
      ItemQuestion q -> qType q == QHidden
      _ -> False
    lookupIdx k xs = case drop k xs of
      (x : _) -> Just x
      [] -> Nothing

-- | One Formbricks block.
data Screen = Screen
  { scId :: Text
  , scTitle :: Maybe I18n
  , scRel :: Maybe Expr
  , scItems :: [Item]
  , scAttrs :: Attrs
  }

-- | Top-level pages as they are; a run of loose items between pages
-- becomes a page of its own.
paginate :: [Item] -> Conv [Group]
paginate items = go (1 :: Int) [] items
  where
    hasPages = not (null [() | ItemGroup g <- items, groupKind g == Page])
    go n acc = \case
      [] -> flush n acc []
      ItemGroup g : rest | groupKind g == Page -> do
        loose <- flush n acc []
        (loose ++) . (g :) <$> go (n + length loose + 1) [] rest
      i : rest -> go n (i : acc) rest
    flush _ [] r = pure r
    flush n acc r = do
      let gid = "page" <> T.pack (show n)
      when hasPages $ warn gid "items outside a page wrapped in a page"
      pure (Group gid Page Nothing Nothing (reverse acc) M.empty : r)

headOr :: a -> [a] -> a
headOr d = \case
  (x : _) -> x
  [] -> d

-- | A page becomes one block, split wherever an item carries its own
-- condition, since Formbricks can only skip whole blocks.
-- | Block ids must be unique; a repeated one gets a numeric suffix.
uniqueScreens :: [Screen] -> Conv [Screen]
uniqueScreens = go S.empty
  where
    go _ [] = pure []
    go seen (sc : rest)
      | S.member (scId sc) seen = do
          let fresh = headOr (scId sc) [c | n <- [2 :: Int ..], let c = scId sc <> "d" <> T.pack (show n), not (S.member c seen)]
          warn (scId sc) ("repeated block id written as " <> fresh)
          (sc {scId = fresh} :) <$> go (S.insert fresh seen) rest
      | otherwise = (sc :) <$> go (S.insert (scId sc) seen) rest

toScreens :: Group -> Conv [Screen]
toScreens g = do
  all' <- concat <$> mapM flattenItem (groupItems g)
  let (flat, dropped) = (filter emits all', filter (not . emits) all')
  mapM_ writeElement dropped
  if null flat
    then [] <$ unless (null all') (warn (groupId g) "page with nothing Formbricks can show dropped")
    else screensOf g flat

-- | Whether 'writeElement' emits an element for the item.
emits :: Item -> Bool
emits = \case
  ItemNote {} -> True
  ItemRaw (Format "formbricks") _ -> True
  ItemQuestion q -> case qType q of
    QHidden -> False
    QCalculate _ -> False
    _ -> True
  _ -> False

screensOf :: Group -> [Item] -> Conv [Screen]
screensOf g flat = do
    let base = blockId (groupId g)
        chunks = chunk flat
    when (base /= groupId g) $ warn (groupId g) ("block id written as " <> base)
    when (length chunks > 1) $
      warn (groupId g) ("page split into " <> T.pack (show (length chunks)) <> " blocks so its conditions can become jumps")
    forM (zip [1 :: Int ..] chunks) $ \(n, (rel, xs)) ->
      pure
        Screen
          { scId = if n == 1 then base else base <> "s" <> T.pack (show n)
          , scTitle = groupTitle g
          , scRel = conj (catMaybes [groupRelevant g, rel])
          , scItems = map strip xs
          , scAttrs = groupAttrs g
          }
  where
    chunk [] = []
    chunk (x : xs) = case itemRel x of
      Just r -> (Just r, [x]) : chunk xs
      Nothing -> let (plainRun, rest) = break (isJust . itemRel) (x : xs) in (Nothing, plainRun) : chunk rest

-- | Nested groups dissolve into their block, their conditions pushed
-- onto their members.
flattenItem :: Item -> Conv [Item]
flattenItem = \case
  ItemGroup g -> do
    warn (groupId g) "nested group flattened into its block"
    kids <- concat <$> mapM flattenItem (groupItems g)
    pure (maybe kids (\r -> map (addRel r) kids) (groupRelevant g))
  i -> pure [i]
  where
    addRel r = \case
      ItemQuestion q -> ItemQuestion q {qRelevant = conj (r : maybe [] pure (qRelevant q))}
      ItemNote i t rel -> ItemNote i t (conj (r : maybe [] pure rel))
      i -> i

-- | An item without its own condition, which travels as jump logic.
strip :: Item -> Item
strip = \case
  ItemQuestion q -> ItemQuestion q {qRelevant = Nothing}
  ItemNote i t _ -> ItemNote i t Nothing
  i -> i

itemRel :: Item -> Maybe Expr
itemRel = \case
  ItemQuestion q -> qRelevant q
  ItemNote _ _ r -> r
  ItemGroup g -> groupRelevant g
  ItemRaw _ _ -> Nothing

-- | Block ids are cuid2-shaped: lowercase letters and digits, starting
-- with a letter.
blockId :: Text -> Text
blockId t =
  let s = T.filter (\c -> isAsciiLower c || isDigit c) (T.map toLower t)
   in case T.uncons s of
        Just (c, _) | isAsciiLower c -> s
        _ -> "b" <> s

i18nJSON :: I18n -> Value
i18nJSON (I18n d tr) = toJSON (M.insert "default" d tr)

writeElement :: Item -> Conv [Value]
writeElement = \case
  ItemNote i t _ ->
    pure [object ["id" .= i, "type" .= ("cta" :: Text), "headline" .= i18nJSON t, "required" .= False, "buttonExternal" .= False]]
  ItemRaw (Format "formbricks") v -> pure [v]
  ItemRaw (Format f) _ -> [] <$ warn "formbricks" ("raw " <> f <> " content dropped")
  ItemGroup g -> [] <$ warn (groupId g) "group dropped"
  ItemQuestion q -> do
    fields <- typeFields q
    when (isJust (qDefault q)) $ warn (qId q) "default value dropped"
    case fields of
      Nothing -> pure []
      Just fs -> do
        let base =
              [ ("id", String (qId q))
              , ("headline", i18nJSON (qLabel q))
              , ("required", Bool (qRequired q))
              ]
                ++ [("subheader", i18nJSON h) | Just h <- [qHint q]]
                ++ fs
            attrs =
              [ (k', x)
              | (k, x) <- M.toList (qAttrs q)
              , "formbricks:" `T.isPrefixOf` k
              , let k' = T.drop 11 k
              , k' `notElem` map fst base
              , k' /= "type"
              ]
        pure [object [(K.fromText k, x) | (k, x) <- base ++ attrs]]

typeFields :: Question -> Conv (Maybe [(Text, Value)])
typeFields q = case qType q of
  QText k -> do
    lim <- textLimit
    pure . Just $
      [("type", "openText"), ("inputType", String (case k of Email -> "email"; Url -> "url"; Phone -> "phone"; _ -> "text"))]
        ++ [("longAnswer", Bool True) | k == LongText]
        ++ lim
  QNumber k -> do
    when (k == Integer) $ warn (qId q) "integer constraint dropped"
    v <- numberRules
    pure (Just ([("type", "openText"), ("inputType", "number")] ++ v))
  QChoice sel style cs
    | typeAttr == Just "pictureSelection" ->
        pure (Just [("type", "pictureSelection"), ("allowMulti", Bool (sel == Multiple)), ("choices", toJSON [object ["id" .= choiceValue c, "imageUrl" .= i18nDefault (choiceLabel c)] | c <- cs])])
    | otherwise -> do
        cs' <- mapM choiceJSON cs
        pure . Just $
          [("type", if sel == Single then "multipleChoiceSingle" else "multipleChoiceMulti"), ("choices", toJSON cs')]
            ++ [("displayType", "dropdown") | style == Dropdown]
  QMatrix sel rows cols -> do
    when (sel == Multiple) $ warn (qId q) "multi-select matrix written as single-select"
    rs <- mapM choiceJSON rows
    cl <- mapM choiceJSON cols
    pure (Just [("type", "matrix"), ("rows", toJSON rs), ("columns", toJSON cl)])
  QScale lo hi l h
    | lo == 0 && hi == 10 && typeAttr `notElem` [Just "rating", Just "csat", Just "ces"] -> pure (Just ([("type", "nps")] ++ labels l h))
    | lo == 1 && hi `elem` [3, 4, 5, 6, 7, 10] ->
        let t = case typeAttr of
              Just x | (x == "csat" && hi == 5) || (x == "ces" && hi `elem` [5, 7]) -> x
              _ -> "rating"
         in pure (Just ([("type", String t), ("range", toJSON hi), ("scale", fromMaybe "number" (M.lookup "formbricks:scale" (qAttrs q)))] ++ labels l h))
    | otherwise -> do
        warn (qId q) ("scale " <> tshow lo <> "-" <> tshow hi <> " written as a number box")
        pure (Just [("type", "openText"), ("inputType", "number"), ("validation", rulesJSON [("minValue", "min", fromIntegral lo), ("maxValue", "max", fromIntegral hi)])])
  QRanking cs -> do
    cs' <- mapM choiceJSON cs
    pure (Just [("type", "ranking"), ("choices", toJSON cs')])
  QDate k -> do
    unless (k == Date) $ warn (qId q) "time part of the date dropped"
    pure (Just [("type", "date"), ("format", fromMaybe "y-M-d" (M.lookup "formbricks:format" (qAttrs q)))])
  QBoolean -> do
    unless (M.member "formbricks:label" (qAttrs q)) $ warn (qId q) "yes/no written as a consent checkbox"
    pure (Just [("type", "consent"), ("label", fromMaybe (i18nJSON (qLabel q)) (M.lookup "formbricks:label" (qAttrs q)))])
  QFile -> pure (Just [("type", "fileUpload"), ("allowMultipleFiles", fromMaybe (Bool False) (M.lookup "formbricks:allowMultipleFiles" (qAttrs q)))])
  QOther t | t `elem` ["cal", "address", "contactInfo"] -> pure (Just [("type", String t)])
  QOther t -> Just [("type", "openText"), ("inputType", "text")] <$ warn (qId q) ("type " <> t <> " written as a text box")
  QGeo -> Just [("type", "openText"), ("inputType", "text")] <$ warn (qId q) "geopoint written as a text box"
  QCalculate _ -> Nothing <$ warn (qId q) "calculated value dropped"
  QHidden -> pure Nothing
  where
    typeAttr = case M.lookup "formbricks:type" (qAttrs q) of
      Just (String t) -> Just t
      _ -> Nothing
    labels l h = [("lowerLabel", i18nJSON x) | Just x <- [l]] ++ [("upperLabel", i18nJSON x) | Just x <- [h]]
    tshow = T.pack . show
    rawValidation = M.lookup "formbricks:validation" (qAttrs q)
    textLimit = case (rawValidation, qValidation q) of
      (Just raw, _) -> pure [("validation", raw)]
      (_, Nothing) -> pure []
      (_, Just v) -> do
        dropped v
        pure $
          if isJust (valMin v) || isJust (valMax v)
            then [("charLimit", obj ["enabled" .=? Bool True, ("min" .=?) =<< num =<< valMin v, ("max" .=?) =<< num =<< valMax v])]
            else []
    numberRules = case (rawValidation, qValidation q) of
      (Just raw, _) -> pure [("validation", raw)]
      (_, Nothing) -> pure []
      (_, Just v) -> do
        dropped v
        let rs = [("minValue", "min", x) | Just x <- [valMin v]] ++ [("maxValue", "max", x) | Just x <- [valMax v]]
        pure [("validation", rulesJSON rs) | not (null rs)]
    rulesJSON rs =
      object
        [ "rules" .= [object ["id" .= ("v" <> T.pack (show n)), "type" .= (t :: Text), "params" .= object [K.fromText p .= num' x]] | (n, (t, p, x)) <- zip [1 :: Int ..] rs]
        , "logic" .= ("and" :: Text)
        ]
    num' = fromMaybe Null . num
    dropped v = do
      when (isJust (valExpr v)) $ warn (qId q) "validation expression dropped"
      when (isJust (valMessage v)) $ warn (qId q) "validation message dropped"
    choiceJSON c = do
      let other = choiceOther c
          i = if other then "other" else choiceValue c
      when (other && choiceValue c /= "other") $ warn (qId q) ("other choice " <> choiceValue c <> " written with id other")
      when (choiceExclusive c && choiceValue c /= "none") $ warn (qId q) ("exclusive choice " <> choiceValue c <> " written as a plain choice")
      pure (object ["id" .= i, "label" .= i18nJSON (choiceLabel c)])

num :: Double -> Maybe Value
num d
  | d == fromIntegral r = Just (toJSON r)
  | otherwise = Just (toJSON d)
  where
    r = round d :: Integer

-- | The condition as it will read back: encoded, decoded and simplified
-- until nothing changes, so a second write reproduces the first.
settle :: Ctx -> Expr -> Expr
settle ctx = go (8 :: Int) . simplify
  where
    names = M.fromList [(i, n) | (n, i) <- M.toList (ctxVars ctx)]
    go 0 e = e
    go n e = case toGroup ctx "x" e >>= readGroup names of
      Right (Just e') | simplify e' /= e -> go (n - 1) (simplify e')
      _ -> e

-- | An expression as a Formbricks condition group.
toGroup :: Ctx -> Text -> Expr -> Either Text Value
toGroup ctx pid e = case e of
  EBin op _ _ | op `elem` [And, Or] -> group op
  ENot x | Just y@(EBin op _ _) <- pushNot x, op `elem` [And, Or] -> toGroup ctx pid y
  _ -> do
    c <- toCond (pid <> "c1") e
    pure (object ["id" .= pid, "connector" .= ("and" :: Text), "conditions" .= [c]])
  where
    group op = do
      cs <- forM (zip [1 :: Int ..] (flatten op e)) $ \(n, x) ->
        let cid = pid <> "c" <> T.pack (show n)
         in case x of
              EBin o _ _ | o `elem` [And, Or], o /= op -> toGroup ctx cid x
              _ -> toCond cid x
      pure (object ["id" .= pid, "connector" .= (if op == And then "and" else "or" :: Text), "conditions" .= cs])
    flatten op x = case x of
      EBin o a b | o == op -> flatten op a ++ flatten op b
      _ -> [x]
    toCond cid x = case x of
      EBin op (ERef q) r | Just o <- lookup op cmpOps -> single cid q o . Just =<< rightOf r
      ESelected q v -> single cid q "includesOneOf" (Just (static (toJSON [v])))
      ENot (ESelected q v) -> single cid q "doesNotIncludeOneOf" (Just (static (toJSON [v])))
      EAnswered q -> single cid q "isSubmitted" Nothing
      ENot (EAnswered q) -> single cid q "isSkipped" Nothing
      ECall o (ERef q : rest) | o `elem` passThrough -> case rest of
        [] -> single cid q o Nothing
        [r] -> single cid q o . Just =<< rightOf r
        _ -> Left ("call " <> o)
      ENot y -> maybe (Left "negation") (toGroupOrCond cid) (pushNot y)
      EBin op _ _ | op `elem` [And, Or] -> toGroup ctx cid x
      _ -> Left (T.pack (take 60 (show x)))
    toGroupOrCond cid y = case y of
      EBin op _ _ | op `elem` [And, Or] -> toGroup ctx cid y
      _ -> toCond cid y
    rightOf = \case
      ELit (LStr s) -> Right (static (String s))
      ELit (LNum n) -> Right (static (fromMaybe Null (num n)))
      ERef r -> Right (operand r)
      other -> Left (T.pack (take 60 (show other)))
    static v = object ["type" .= ("static" :: Text), "value" .= v]
    single cid q o r = Right . obj $ ["id" .=? String cid, "leftOperand" .=? operand q, "operator" .=? String o] ++ [("rightOperand" .=?) =<< r]
    operand q
      | q `elem` ctxHidden ctx = object ["type" .= ("hiddenField" :: Text), "value" .= q]
      | Just i <- M.lookup q (ctxVars ctx) = object ["type" .= ("variable" :: Text), "value" .= i]
      | Just r <- T.stripPrefix "reserved:" q = object ["type" .= ("reserved" :: Text), "value" .= r]
      | otherwise = object ["type" .= ("element" :: Text), "value" .= q]
    cmpOps = [(Eq, "equals"), (Neq, "doesNotEqual"), (Gt, "isGreaterThan"), (Lt, "isLessThan"), (Ge, "isGreaterThanOrEqual"), (Le, "isLessThanOrEqual")]
    passThrough =
      [ "contains", "doesNotContain", "startsWith", "doesNotStartWith", "endsWith", "doesNotEndWith", "isClicked", "isNotClicked"
      , "isAccepted", "isBefore", "isAfter", "isBooked", "isPartiallySubmitted", "isCompletelySubmitted", "isSet", "isNotSet"
      , "isEmpty", "isNotEmpty"
      ]

