-- | Triple-S XML 2.0 / 3.0 (triple-s.org): a market-research data
-- description. Each @variable@ is a data column of type single, multiple,
-- quantity, character, logical, date or time, with coded @values@ and a
-- fixed-width @position@. There are no pages, routing or question texts
-- beyond the label; the only condition is a @filter@ naming a logical
-- variable.
module Panform.Format.TripleS (readTripleS, writeTripleS) where

import Control.Monad (unless, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Format.Codebook
import Panform.XmlKit

------------------------------------------------------------------------------
-- Reading

readTripleS :: BL.ByteString -> Conv Form
readTripleS bs = do
  root <- parseXml "Triple-S XML" bs
  unless (localName root == "sss") $ failWith "Triple-S: root element is not <sss>"
  survey <- maybe (failWith "Triple-S: missing <survey>") pure (child "survey" root)
  let defLang = langOf root
      records = children "record" survey
  when (length records > 1) $ warn "triple-s" "hierarchical records merged into one variable list"
  qs <- mapM (readVariable defLang) (concatMap (children "variable") records)
  let langs = words' (attr "languages" root)
      title = readText defLang <$> child "title" survey
      -- the name the writer gives a survey without an id
      defaultName = maybe "survey" (slug . i18nDefault) title
      header = [("triple-s:" <> k, String v) | k <- ["date", "time", "origin", "user"], Just e <- [child k root], let v = textOf e, not (T.null v)]
      ver = [("triple-s:version", String v) | Just e <- [child "version" survey], let v = textOf e, not (T.null v)]
      meta =
        emptyMeta
          { metaId = (child "name" survey >>= nonBlank . textOf) >>= \n -> if n == defaultName then Nothing else Just n
          , metaTitle = title
          , metaLanguages = maybe [] (\d -> d : filter (/= d) langs) defLang
          , metaAttrs = M.fromList (header ++ ver)
          }
  pure (Form meta (map ItemQuestion qs))
  where
    words' = T.words

-- | A label, title or value: direct text is the default language, and
-- @<text xml:lang>@ children carry translations. Of several texts for one
-- language (interview and analysis modes) the interview text, which is the
-- question as asked, wins.
readText :: Maybe Lang -> Element -> I18n
readText defLang e =
  let texts = sortOn modeRank (children "text" e)
      direct
        | null texts = exactText e
        | otherwise = deepTextExcept ["text"] e
      isDefault t = maybe True (\l -> Just l == defLang) (langOf t)
      interview = [t | t <- texts, isDefault t, attr "mode" t == "interview"]
      dflt = case interview of
        (t : _) -> exactText t
        []
          | not (T.null direct) -> direct
          | otherwise -> maybe "" exactText (listToMaybe (filter isDefault texts))
      trans = M.fromListWith (\_ old -> old) [(l, exactText t) | t <- texts, Just l <- [langOf t], Just l /= defLang]
   in I18n dflt trans
  where
    modeRank t = if attr "mode" t == "analysis" then 1 else 0 :: Int

-- | The range the writer gives a number with no bounds of its own.
isDefaultRange :: Text -> Text -> Bool
isDefaultRange a b = readNum a == Just 0 && readNum b `elem` [Just 999999, Just 999999.99]

readVariable :: Maybe Lang -> Element -> Conv Question
readVariable defLang v = do
  let name = fromMaybe ("V" <> attr "ident" v) (nonBlank . exactText =<< child "name" v)
      label = maybe (plain "") (readText defLang) (child "label" v)
      vals = maybe [] (children "value") (child "values" v)
      codes = [(attr "code" x, readText defLang x) | x <- vals]
      range = do
        r <- child "values" v >>= child "range"
        pure (attr "from" r, attr "to" r)
      size = child "size" v >>= readNum . textOf
      width = do
        p <- child "position" v
        s <- readNum (attr "start" p)
        f <- readNum (attr "finish" p)
        pure (f - s + 1)
      use = nonBlank (attr "use" v)
      ty = attr "type" v
  when (any (isJust . attrMaybe "score") vals) $ warn name "value scores dropped"
  when (any (isJust . attrMaybe "mode") (maybe [] (children "text") (child "label" v))) $
    warn name "alternative interview/analysis label dropped"
  qt <- case ty of
    "single" -> pure (fromMaybe (QChoice Single Radio (map mkChoice codes)) (scaleFromCodes codes))
    "multiple" -> pure (QChoice Multiple Radio (map mkChoice codes))
    "quantity"
      | isJust use -> pure QHidden
      | otherwise -> do
          unless (null codes) $ warn name "labelled values on a quantity dropped"
          pure (QNumber (if maybe False (\(a, b) -> "." `T.isInfixOf` (a <> b)) range then Decimal else Integer))
    "character" -> pure (QText (if fromMaybe 0 (size `orElse` width) > 255 then LongText else ShortText))
    "logical" -> pure QBoolean
    "date" -> pure (QDate Date)
    "time" -> pure (QDate Time)
    other -> QOther other <$ warn name ("unknown variable type " <> other)
  when (ty `elem` ["date", "time"] && isJust range) $ warn name "date/time range dropped"
  let validation = case (qt, range) of
        (QNumber _, Just (a, b)) | not (isDefaultRange a b) -> Just (Validation (readNum a) (readNum b) Nothing Nothing)
        _ -> Nothing
      decimals = case (qt, range) of
        (QNumber Decimal, Just (a, _)) -> [("triple-s:decimals", String (tshow (T.length (T.drop 1 (T.dropWhile (/= '.') a)))))]
        _ -> []
      hidden = case (use, range) of
        (Just u, r) -> [("triple-s:use", String u)] ++ [("triple-s:range", String (a <> ".." <> b)) | Just (a, b) <- [r]]
        _ -> []
      filterRef = nonBlank . textOf =<< child "filter" v
  pure
    (question name "" qt)
      { qLabel = label
      , qValidation = validation
      , qRelevant = (\f -> EBin Eq (ERef f) (ELit (LNum 1))) <$> filterRef
      , qAttrs = M.fromList (decimals ++ hidden)
      }
  where
    mkChoice (c, l) = Choice c l False False
    orElse a b = maybe b Just a

------------------------------------------------------------------------------
-- Writing

-- | One Triple-S variable before positions are assigned.
data Var = Var
  { vName :: Text
  , vType :: Text
  , vLabel :: I18n
  , vLiteral :: Bool
  , vUse :: Maybe Text
  , vValues :: [(Text, I18n)]
  , vRange :: Maybe (Text, Text)
  , vSpread :: Maybe (Int, Int)
  -- ^ Subfields and width, for a multiple in spread layout.
  , vSize :: Maybe Int
  , vFilter :: Maybe Text
  , vWidth :: Int
  }

writeTripleS :: Form -> Conv BL.ByteString
writeTripleS (Form meta items) = do
  qs <- concat <$> (flattenItems "triple-s" items >>= mapM expandQuestion)
  when (isJust (metaDescription meta)) $ warn "triple-s" "survey description dropped"
  let logicals = [qId q | q <- qs, qType q == QBoolean]
  vars <- concat <$> mapM (varOf logicals) qs
  let defLang = listToMaybe (metaLanguages meta)
      langs = nub (maybe [] pure defLang ++ sort (nub (concatMap i18nLangs (maybe [] pure (metaTitle meta) ++ concatMap (\x -> vLabel x : map snd (vValues x)) vars))))
      header = [leaf k [] v | k <- ["date", "time", "origin", "user"], Just (String v) <- [M.lookup ("triple-s:" <> k) (metaAttrs meta)]]
      name = fromMaybe (maybe "survey" (slug . i18nDefault) (metaTitle meta)) (metaId meta)
      survey =
        el "survey" [] $
          [leaf "name" [] name]
            ++ [leaf "version" [] v | Just (String v) <- [M.lookup "triple-s:version" (metaAttrs meta)]]
            ++ [el "title" [] (texts defLang t) | Just t <- [metaTitle meta]]
            ++ [el "record" [("ident", "V")] (zipWith3 (variable defLang) [1 :: Int ..] (positions vars) vars)]
      rootAttrs =
        [("version", "3.0")]
          ++ [("xml:lang", l) | Just l <- [defLang], multi]
          ++ [("languages", T.unwords langs) | multi]
      multi = any (\l -> Just l /= defLang) langs
  pure (renderXml (el "sss" rootAttrs (header ++ [survey])))

positions :: [Var] -> [Int]
positions = scanl (+) 1 . map vWidth

-- | Plain text, or one @<text>@ per language when there are translations
-- (the untagged one is the default).
texts :: Maybe Lang -> I18n -> [Node]
texts defLang t0
  | M.null (i18nTranslations t) = [Tx (i18nDefault t)]
  | otherwise = leaf "text" [] (i18nDefault t) : [leaf "text" [("xml:lang", l)] x | (l, x) <- M.toList (i18nTranslations t)]
  where
    -- a "translation" into the default language is the default text
    t = t0 {i18nTranslations = M.filterWithKey (\l _ -> Just l /= defLang) (i18nTranslations t0)}

variable :: Maybe Lang -> Int -> Int -> Var -> Node
variable defLang ident start v =
  el "variable" ([("ident", tshow ident), ("type", vType v)] ++ [("format", "literal") | vLiteral v] ++ [("use", u) | Just u <- [vUse v]]) $
    [ leaf "name" [] (if T.null (T.strip (vName v)) then "V" <> tshow ident else vName v)
    , el "label" [] (texts defLang (vLabel v))
    , el "position" [("start", tshow start), ("finish", tshow (start + vWidth v - 1))] []
    ]
      ++ [leaf "size" [] (tshow s) | Just s <- [vSize v]]
      ++ [leaf "filter" [] f | Just f <- [vFilter v]]
      ++ [el "spread" [("subfields", tshow n), ("width", tshow w)] [] | Just (n, w) <- [vSpread v]]
      ++ [ el "values" [] $
             [el "range" [("from", a), ("to", b)] [] | Just (a, b) <- [vRange v]]
               ++ [el "value" [("code", c)] (texts defLang l) | (c, l) <- vValues v]
         | isJust (vRange v) || not (null (vValues v))
         ]

varOf :: [Text] -> Question -> Conv [Var]
varOf logicals q = do
  lossWarnings (Loses True True False True False) q
  when (T.null (T.strip (qId q))) $ warn "triple-s" "blank variable name replaced by V and its position"
  when (isJust (qValidation q >>= valMessage)) $ warn (qId q) "validation message dropped"
  filt <- case qRelevant q of
    Nothing -> pure Nothing
    Just (EBin Eq (ERef f) (ELit (LNum 1))) | f `elem` logicals -> pure (Just f)
    Just _ -> Nothing <$ warn (qId q) "display logic dropped (a Triple-S filter can only name a logical variable)"
  let base = Var (qId q) "character" (qLabel q) False Nothing [] Nothing Nothing Nothing filt 0
      character n = base {vSize = Just n, vWidth = n}
      coded ty cs = codedVar base ty cs
  case qType q of
    QText ShortText -> pure [character 255]
    QText LongText -> pure [character 1000]
    QText k -> [character 255] <$ warn (qId q) (T.toLower (tshow k) <> " validation dropped")
    QNumber k -> do
      let dec = case M.lookup "triple-s:decimals" (qAttrs q) of
            Just (String d) | Just n <- readNum d -> round n
            _ -> 2
          fmt = case k of
            Integer -> fmtNum . fromIntegral . (round :: Double -> Integer)
            Decimal -> fmtFixed dec
          lo = fmt (fromMaybe 0 (qValidation q >>= valMin))
          hi = fmt (fromMaybe (if k == Integer then 999999 else 999999.99) (qValidation q >>= valMax))
      pure [base {vType = "quantity", vRange = Just (lo, hi), vWidth = max (T.length lo) (T.length hi)}]
    QChoice sel style cs -> do
      when (style /= Radio) $ warn (qId q) (T.toLower (tshow style) <> " display style dropped")
      when (any choiceOther cs) $ warn (qId q) "\"other, specify\" text box dropped"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive (none of the above) flag dropped"
      pure [coded (if sel == Single then "single" else "multiple") [(choiceValue c, choiceLabel c) | c <- cs]]
    QScale lo hi l h -> pure [coded "single" (scaleCodes lo hi l h)]
    QBoolean -> pure [base {vType = "logical", vWidth = 1}]
    QDate Date -> pure [base {vType = "date", vWidth = 8}]
    QDate Time -> pure [base {vType = "time", vWidth = 6}]
    QDate DateTime -> [base {vType = "date", vWidth = 8}] <$ warn (qId q) "date-time written as a date"
    QHidden
      | Just (String u) <- M.lookup "triple-s:use" (qAttrs q) -> do
          let (a, b) = case M.lookup "triple-s:range" (qAttrs q) of
                Just (String r) | (x, y) <- T.breakOn ".." r, not (T.null y) -> (x, T.drop 2 y)
                _ -> ("0", "999999")
          pure [base {vType = "quantity", vUse = Just u, vRange = Just (a, b), vWidth = max (T.length a) (T.length b)}]
      | otherwise -> [character 255] <$ warn (qId q) "hidden variable written as character"
    QCalculate _ -> [character 255] <$ warn (qId q) "calculation dropped; written as a character variable"
    QFile -> [character 255] <$ warn (qId q) "file upload written as a character variable"
    QGeo -> [character 255] <$ warn (qId q) "geopoint written as a character variable"
    QOther t -> [character 255] <$ warn (qId q) ("type " <> t <> " written as a character variable")
    QMatrix {} -> pure []
    QRanking _ -> pure []

-- | Numeric codes when every code is a non-negative integer, otherwise
-- @format="literal"@. A numeric multiple is a bit string (one column per
-- code up to the highest); a literal one, or one with a code 0, is spread.
codedVar :: Var -> Text -> [(Text, I18n)] -> Var
codedVar base ty cs =
  let literal = not (all (isCode . fst) cs)
      codeWidth = maximum (1 : map (T.length . fst) cs)
      maxCode = maximum (1 : mapMaybe (fmap round . readNum . fst) cs) :: Int
      bitString = not literal && all ((/= Just 0) . readNum . fst) cs
      v = base {vType = ty, vLiteral = literal, vValues = cs}
   in case ty of
        "multiple"
          | bitString -> v {vWidth = maxCode}
          | otherwise -> v {vSpread = Just (length cs, codeWidth), vWidth = max 1 (length cs * codeWidth)}
        _ -> v {vWidth = codeWidth}
