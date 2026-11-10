-- | LimeSurvey's tab-separated survey structure (Export > "Tab-separated
-- values"). Each row has a @class@: @S@ survey setting, @SL@ language
-- setting, @G@ group, @Q@ question, @SQ@ sub-question, @A@ answer, @C@
-- legacy condition. Nesting follows row order, and the whole structure is
-- repeated once per language; question attributes are extra columns.
module Panform.Format.LimeSurveyTsv (readLimeSurveyTsv, writeLimeSurveyTsv) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Format.LimeSurvey

type Row = M.Map Text Text

-- | The columns LimeSurvey's exporter writes before the attribute columns.
baseColumns :: [Text]
baseColumns =
  [ "id"
  , "related_id"
  , "class"
  , "type/scale"
  , "name"
  , "relevance"
  , "text"
  , "help"
  , "language"
  , "validation"
  , "mandatory"
  , "encrypted"
  , "other"
  , "default"
  , "same_default"
  , "question_theme_name"
  , "same_script"
  ]

-- | Columns read by name rather than kept as attributes.
fixedColumns :: [Text]
fixedColumns = baseColumns ++ ["random_group", "assessment_value"]

-- | Question columns that are fields rather than attributes.
fieldColumns :: [Text]
fieldColumns = ["encrypted", "same_default", "same_script"]

get :: Text -> Row -> Text
get k = M.findWithDefault "" k

------------------------------------------------------------------------------
-- Reading

readLimeSurveyTsv :: BL.ByteString -> Conv Form
readLimeSurveyTsv bs = do
  rows <- case parseTsv (TE.decodeUtf8Lenient (BL.toStrict bs)) of
    [] -> failWith "LimeSurvey TSV is empty"
    (hdr : body) -> do
      -- Attribute names keep their case (dualscale_headerA); the fixed
      -- columns match in any case.
      let keys = [let k = T.strip h in if T.toLower k `elem` fixedColumns then T.toLower k else k | h <- hdr]
      unless (all (`elem` keys) ["class", "name", "text"]) $ failWith "LimeSurvey TSV needs class, name and text columns"
      pure [M.fromList (filter (not . T.null . snd) (zip keys r)) | r <- body, any (not . T.null) r]
  let settings = [(get "name" r, get "text" r) | r <- rows, get "class" r == "S"]
      base = fromMaybe "en" (lookup "language" settings)
      langs = nub (base : T.words (fromMaybe "" (lookup "additional_languages" settings)))
      langOf r = let l = get "language" r in if T.null l then base else l
      slRows = [(get "name" r, (langOf r, get "text" r)) | r <- rows, get "class" r == "SL", get "name" r `notElem` ["surveyls_language", "surveyls_survey_id"]]
      langSettings = [(k, i18nFrom base [v | (k', v) <- slRows, k' == k]) | k <- nub (map fst slRows)]
      structure = [r | r <- rows, get "class" r `elem` ["G", "Q", "SQ", "A", "C"]]
      perLang l = [r | r <- structure, langOf r == l]
      groupsIn l = buildGroups (perLang l)
  forM_ (nub [c | r <- rows, let c = get "class" r, c `notElem` ["S", "SL", "G", "Q", "SQ", "A", "C"]]) $ \c ->
    warn "limesurvey" ("rows of class " <> c <> " (assessments, quotas) dropped")
  let groups = foldl' (\gs l -> zipWithLang l gs (groupsIn l)) (groupsIn base) (filter (/= base) langs)
  fromLs
    LsSurvey
      { lsId = fromMaybe "" (lookup "sid" settings)
      , lsLangs = langs
      , lsSettings = [(k, v) | (k, v) <- settings, not (T.null v), k `notElem` ["sid", "language", "additional_languages"]]
      , lsLangSettings = langSettings
      , lsGroups = groups
      }

-- | One language's rows into groups; each row belongs to the group or
-- question above it.
buildGroups :: [Row] -> [LsGroup]
buildGroups = reverse . map finishGroup . foldl' step []
  where
    step gs r = case (get "class" r, gs) of
      ("G", _) ->
        LsGroup (get "type/scale" r) (plain (get "name" r)) (plain (get "text" r)) (get "relevance" r) (get "random_group" r) [] : gs
      ("Q", g : rest) -> g {lgQuestions = newQuestion r : lgQuestions g} : rest
      (c, g : rest) | (q : qs) <- lgQuestions g -> g {lgQuestions = addTo c r q : qs} : rest
      _ -> gs
    finishGroup g = g {lgQuestions = reverse (map finishQ (lgQuestions g))}
    finishQ q = q {lqSubs = reverse (lqSubs q), lqAnswers = reverse (lqAnswers q), lqConds = reverse (lqConds q)}
    newQuestion r =
      (lsQuestion (get "type/scale" r) (get "name" r))
        { lqId = get "id" r
        , lqText = plain (get "text" r)
        , lqHelp = plain (get "help" r)
        , lqRelevance = get "relevance" r
        , lqPreg = get "validation" r
        , lqMandatory = orDefault "N" (get "mandatory" r)
        , lqOther = orDefault "N" (get "other" r)
        , lqTheme = get "question_theme_name" r
        , lqDefault = plain <$> nonEmpty (get "default" r)
        , lqFields = [(k, v) | k <- fieldColumns, let v = get k r, v `notElem` ["", "N", "0"]]
        , lqAttrs = [(k, plain v) | (k, v) <- M.toList r, k `notElem` baseColumns]
        }
    addTo c r q = case c of
      "SQ"
        | lqType q `elem` ["O", "|"] -> q
        | get "name" r == "other" && lqOther q == "Y" -> q
        | otherwise -> q {lqSubs = LsSub (get "id" r) (get "name" r) (scale r) (plain (get "text" r)) (get "relevance" r) (plain <$> nonEmpty (get "default" r)) : lqSubs q}
      "A" -> q {lqAnswers = LsAnswer (get "name" r) (scale r) (plain (get "text" r)) (get "assessment_value" r) : lqAnswers q}
      "C" -> q {lqConds = LsCond (get "type/scale" r) (get "related_id" r) (get "name" r) (get "relevance" r) (get "text" r) : lqConds q}
      _ -> q
    scale r = fromMaybe 0 (readInt (get "type/scale" r))
    orDefault d t = if T.null t then d else t

-- | Fold a translation's groups into the base language's: groups match by
-- position, questions by code, sub-questions and answers by scale and code.
zipWithLang :: Lang -> [LsGroup] -> [LsGroup] -> [LsGroup]
zipWithLang l base tr = zipWith mergeG base (map Just tr ++ repeat Nothing)
  where
    mergeG g Nothing = g
    mergeG g (Just t) =
      g
        { lgName = mergeI18n l (i18nDefault (lgName t)) (lgName g)
        , lgDesc = mergeI18n l (i18nDefault (lgDesc t)) (lgDesc g)
        , lgQuestions = [maybe q (mergeQ q) (lookup k (occurrences (lgQuestions t))) | (k, q) <- occurrences (lgQuestions g)]
        }
    mergeQ q t =
      q
        { lqText = mergeI18n l (i18nDefault (lqText t)) (lqText q)
        , lqHelp = mergeI18n l (i18nDefault (lqHelp t)) (lqHelp q)
        , lqDefault = case (lqDefault q, lqDefault t) of
            (Just d, Just dt) -> Just (mergeI18n l (i18nDefault dt) d)
            (d, _) -> d
        , lqAttrs = mergeAttrs (lqAttrs q) (lqAttrs t)
        , lqSubs = [maybe s (\st -> s {sqText = mergeI18n l (i18nDefault (sqText st)) (sqText s)}) (lookupBy key (key s) (lqSubs t)) | s <- lqSubs q]
        , lqAnswers = [maybe a (\at -> a {aText = mergeI18n l (i18nDefault (aText at)) (aText a)}) (lookupBy akey (akey a) (lqAnswers t)) | a <- lqAnswers q]
        }
    -- A repeated code matches the same repeat in the translation.
    occurrences qs = [((lqTitle q, length [() | p <- take i qs, lqTitle p == lqTitle q]), q) | (i, q) <- zip [0 :: Int ..] qs]
    key s = (sqScale s, sqCode s)
    akey a = (aScale a, aCode a)
    mergeAttrs as ts =
      [(k, maybe v (\t -> mergeI18n l (i18nDefault t) v) (lookup k ts)) | (k, v) <- as]
        ++ [(k, I18n "" (M.singleton l (i18nDefault t))) | (k, t) <- ts, k `notElem` map fst as]
    lookupBy f k xs = case filter ((== k) . f) xs of
      (x : _) -> Just x
      [] -> Nothing

-- | Tab-separated lines, forgiving the way PHP's @fgetcsv@ is: a field
-- that starts with a double quote runs to the matching quote (@""@ is a
-- literal quote) and may span lines; any other field is taken as is.
parseTsv :: Text -> [[Text]]
parseTsv = go . stripBom
  where
    stripBom t = fromMaybe t (T.stripPrefix "\xFEFF" t)
    go t
      | T.null t = []
      | otherwise = let (r, rest) = line t [] in r : go rest
    line t acc =
      let (f, rest) = field t
          acc' = f : acc
       in case T.uncons rest of
            Just ('\t', r) -> line r acc'
            Just ('\r', r) -> (reverse acc', fromMaybe r (T.stripPrefix "\n" r))
            Just ('\n', r) -> (reverse acc', r)
            _ -> (reverse acc', "")
    field t = case T.uncons t of
      Just ('"', r) -> quoted r ""
      _ -> T.break (`elem` ['\t', '\n', '\r']) t
    quoted t acc =
      let (chunk, rest) = T.break (== '"') t
       in case T.uncons (T.drop 1 rest) of
            _ | T.null rest -> (acc <> chunk, "")
            Just ('"', r) -> quoted r (acc <> chunk <> "\"")
            _ -> let (tailPart, after) = T.break (`elem` ['\t', '\n', '\r']) (T.drop 1 rest) in (acc <> chunk <> tailPart, after)

------------------------------------------------------------------------------
-- Writing

writeLimeSurveyTsv :: Form -> Conv BL.ByteString
writeLimeSurveyTsv s = do
  ls0 <- toLs s
  -- An attribute named like a fixed column would read back as that column.
  let clash (k, _) = T.toLower k `elem` fixedColumns
  forM_ [(lqTitle q, k) | g <- lsGroups ls0, q <- lgQuestions g, a@(k, _) <- lqAttrs q, clash a] $ \(t, k) ->
    warn t ("attribute " <> k <> " dropped; it clashes with a column of the same name")
  let ls = ls0 {lsGroups = [g {lgQuestions = [q {lqAttrs = filter (not . clash) (lqAttrs q)} | q <- lgQuestions g]} | g <- lsGroups ls0]}
  let langs = lsLangs ls
      base = fromMaybe "en" (safeHead' langs)
      settingRows =
        [row [("class", "S"), ("name", "sid"), ("text", lsId ls)] | not (T.null (lsId ls))]
          ++ [row [("class", "S"), ("name", "language"), ("text", base)]]
          ++ [row [("class", "S"), ("name", "additional_languages"), ("text", T.unwords (drop 1 langs))] | length langs > 1]
          ++ [row [("class", "S"), ("name", k), ("text", v)] | (k, v) <- lsSettings ls]
      langRows = [row [("class", "SL"), ("name", k), ("text", (i18nText (Just l) v)), ("language", l)] | l <- langs, (k, v) <- lsLangSettings ls]
      structure = concat [groupRows l g | l <- langs, g <- lsGroups ls]
      attrCols = sort (nub [k | g <- lsGroups ls, q <- lgQuestions g, (k, _) <- lqAttrs q])
      extra = ["random_group" | any (not . T.null . lgRandom) (lsGroups ls), "random_group" `notElem` attrCols] ++ ["assessment_value" | any (not . T.null . aAssess) (concatMap lqAnswers (concatMap lgQuestions (lsGroups ls)))]
  when (any (not . null . lqConds) (concatMap lgQuestions (lsGroups ls))) $ warn "limesurvey" "legacy conditions dropped"
  pure (render (baseColumns ++ attrCols ++ extra) (settingRows ++ langRows ++ structure))
  where
    row = M.fromList
    -- The importer only uses type/scale to match a group across languages.
    groupRows l g =
      row
        [ ("class", "G")
        , ("type/scale", lgId g)
        , ("name", i18nText (Just l) (lgName g))
        , ("relevance", lgRelevance g)
        , ("text", i18nText (Just l) (lgDesc g))
        , ("language", l)
        , ("random_group", lgRandom g)
        ]
        : concatMap (questionRows l) (lgQuestions g)
    questionRows l q =
      row
        ( [ ("class", "Q")
          , ("type/scale", lqType q)
          , ("name", lqTitle q)
          , ("relevance", lqRelevance q)
          , ("text", i18nText (Just l) (lqText q))
          , ("help", i18nText (Just l) (lqHelp q))
          , ("language", l)
          , ("validation", lqPreg q)
          , ("mandatory", lqMandatory q)
          , ("other", lqOther q)
          , ("default", maybe "" (i18nText (Just l)) (lqDefault q))
          , ("question_theme_name", lqTheme q)
          ]
            ++ lqFields q
            ++ [(k, (i18nText (Just l) v)) | (k, v) <- lqAttrs q]
        )
        : [ row
              [ ("class", "SQ")
              , ("type/scale", if sqScale sq == 0 then "" else tshow' (sqScale sq))
              , ("name", sqCode sq)
              , ("relevance", sqRelevance sq)
              , ("text", i18nText (Just l) (sqText sq))
              , ("language", l)
              , ("default", maybe "" (i18nText (Just l)) (sqDefault sq))
              ]
          | sq <- lqSubs q
          ]
          ++ [ row
                 [ ("class", "A")
                 , ("type/scale", tshow' (aScale a))
                 , ("name", aCode a)
                 , ("text", i18nText (Just l) (aText a))
                 , ("language", l)
                 , ("assessment_value", aAssess a)
                 ]
             | a <- lqAnswers q
             ]

render :: [Text] -> [Row] -> BL.ByteString
render cols rows = BL.fromStrict . TE.encodeUtf8 $ T.concat [T.intercalate "\t" (map quote r) <> "\n" | r <- cols : [[get c r | c <- cols] | r <- rows]]
  where
    quote t
      | T.any (`elem` ['\t', '\n', '\r']) t || "\"" `T.isPrefixOf` t = "\"" <> T.replace "\"" "\"\"" t <> "\""
      | otherwise = t

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing

safeHead' :: [a] -> Maybe a
safeHead' = \case
  (x : _) -> Just x
  [] -> Nothing

tshow' :: Show a => a -> Text
tshow' = T.pack . show
