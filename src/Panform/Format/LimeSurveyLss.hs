-- | LimeSurvey survey structure (@.lss@): an XML dump of the survey's
-- database tables, each as @<fields>@ plus @<rows><row>@. Before LimeSurvey
-- 4 (DBVersion < 400) texts sit in the structural tables, one row per
-- language; from 4 on they live in @*_l10ns@ tables. Both are read; the
-- writer emits the 5.x/6.x layout.
module Panform.Format.LimeSurveyLss (readLimeSurveyLss, writeLimeSurveyLss) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.XML as X
import Panform.AST
import Panform.Conv
import Panform.Format.LimeSurvey

type Row = M.Map Text Text

get :: Text -> Row -> Text
get k = M.findWithDefault "" k

-- | The DBVersion written: LimeSurvey 6.x.
dbVersion :: Text
dbVersion = "641"

------------------------------------------------------------------------------
-- Reading

readLimeSurveyLss :: BL.ByteString -> Conv Form
readLimeSurveyLss bs = do
  doc <- either (Left . show) Right (X.parseLBS X.def bs) `orFail` "LimeSurvey .lss XML"
  let root = X.documentRoot doc
      children = elements root
      textOf n = maybe "" content (lookup n [(local e, e) | e <- children])
      tables = M.fromList [(local e, tableRows e) | e <- children, any ((== "rows") . local) (elements e)]
      table n = M.findWithDefault [] n tables
      has n = not (null (table n))
  unless (textOf "LimeSurveyDocType" == "Survey") $ failWith "not a LimeSurvey survey structure (.lss) file"
  let survey = fromMaybe M.empty (safeHead (table "surveys"))
      listed = [content l | e <- children, local e == "languages", l <- elements e, local l == "language"]
      base = let l = get "language" survey in if T.null l then fromMaybe "en" (safeHead listed) else l
      langs = nub (base : listed ++ T.words (get "additional_languages" survey))
      i18n rows f = i18nFrom base [(langOf r, get f r) | r <- rows]
      langOf r = let l = get "language" r in if T.null l then base else l
      byKey k rows = collect [(get k r, [r]) | r <- rows]
      -- Structure and texts: separate tables from 4.x on, one table before.
      groupRows = table "groups"
      groupTexts = byKey "gid" (if has "group_l10ns" then table "group_l10ns" else groupRows)
      qRows = table "questions" ++ table "subquestions"
      qTexts = byKey "qid" (if has "question_l10ns" then table "question_l10ns" else qRows)
      qFirst = M.fromListWith (\_ a -> a) [(get "qid" r, r) | r <- qRows]
      subsOf = collect [(get "parent_qid" r, [r]) | r <- M.elems qFirst, not (isMain r)]
      isMain r = get "parent_qid" r `elem` ["", "0"]
      answersOf
        | has "answer_l10ns" =
            let texts = byKey "aid" (table "answer_l10ns")
             in collect [(get "qid" r, [(r, M.findWithDefault [] (get "aid" r) texts)]) | r <- table "answers"]
        | otherwise =
            let keyed = collect [((get "qid" r, get "scale_id" r, get "code" r), [r]) | r <- table "answers"]
             in collect [(q, [(r, rs)]) | ((q, _, _), rs@(r : _)) <- M.toList keyed]
      attrsOf = collect [(get "qid" r, [r]) | r <- table "question_attributes"]
      defaults
        | has "defaultvalue_l10ns" =
            let texts = byKey "dvid" (table "defaultvalue_l10ns")
             in [(r, M.findWithDefault [] (get "dvid" r) texts) | r <- table "defaultvalues"]
        | otherwise = [(r, rs) | rs@(r : _) <- M.elems (byKey3 (table "defaultvalues"))]
      byKey3 rows = collect [((get "qid" r, get "sqid" r, get "scale_id" r, get "specialtype" r), [r]) | r <- rows]
      defaultFor qid sqid = [i18n rs "defaultvalue" | (r, rs) <- defaults, get "qid" r == qid, get "sqid" r `elem` sqid, T.null (get "specialtype" r)]
      condsOf = collect [(get "qid" r, [LsCond (get "scenario" r) (get "cqid" r) (get "cfieldname" r) (get "method" r) (get "value" r)]) | r <- table "conditions"]
      attrI18n rs = sortOn fst [(a, i18nFrom base [(langOf r, get "value" r) | r <- ars]) | (a, ars) <- M.toList (byKey "attribute" rs), any (not . T.null . get "value") ars]
      question r =
        let qid = get "qid" r
            texts = M.findWithDefault [] qid qTexts
            subs = sortOn (\s -> (readInt (get "scale_id" s), readInt (get "question_order" s))) (M.findWithDefault [] qid subsOf)
            answers = sortOn (\(a, _) -> (readInt (get "scale_id" a), readInt (get "sortorder" a))) (M.findWithDefault [] qid answersOf)
         in ( [get "title" r | any (not . T.null . get "script") texts]
            , (lsQuestion (get "type" r) (get "title" r))
                { lqId = qid
                , lqText = i18n texts "question"
                , lqHelp = i18n texts "help"
                , lqRelevance = get "relevance" r
                , lqPreg = get "preg" r
                , lqMandatory = orDefault "N" (get "mandatory" r)
                , lqOther = orDefault "N" (get "other" r)
                , lqTheme = get "question_theme_name" r
                , lqDefault = safeHead (defaultFor qid ["", "0"])
                , lqFields = [(k, v) | k <- ["encrypted", "same_default", "same_script", "modulename"], let v = get k r, v `notElem` ["", "N", "0"]]
                , lqAttrs = attrI18n (M.findWithDefault [] qid attrsOf)
                , lqSubs =
                    [ LsSub (get "qid" s) (get "title" s) (fromMaybe 0 (readInt (get "scale_id" s))) (i18n (M.findWithDefault [] (get "qid" s) qTexts) "question") (get "relevance" s) (safeHead (defaultFor qid [get "qid" s]))
                    | s <- subs
                    ]
                , lqAnswers = [LsAnswer (get "code" a) (fromMaybe 0 (readInt (get "scale_id" a))) (i18n ts "answer") (get "assessment_value" a) | (a, ts) <- answers]
                , lqConds = M.findWithDefault [] qid condsOf
                }
            )
      mains = sortOn (\r -> readInt (get "question_order" r)) (filter isMain (M.elems qFirst))
      -- By group_order, then by first appearance.
      firstSeen = M.fromListWith min [(get "gid" r, (readInt (get "group_order" r), n)) | (n, r) <- zip [0 :: Int ..] groupRows]
      gidOrder = map fst (sortOn snd (M.toList firstSeen))
      groupFirst = M.fromListWith (\_ a -> a) [(get "gid" r, r) | r <- groupRows]
      built = [(get "gid" r, question r) | r <- mains]
      groups =
        [ LsGroup
            { lgId = gid
            , lgName = i18n texts "group_name"
            , lgDesc = i18n texts "description"
            , lgRelevance = get "grelevance" g
            , lgRandom = get "randomization_group" g
            , lgQuestions = [q | (qg, (_, q)) <- built, qg == gid]
            }
        | gid <- gidOrder
        , let g = M.findWithDefault M.empty gid groupFirst
              texts = M.findWithDefault [] gid groupTexts
        ]
      slRows = table "surveys_languagesettings"
      slKeys = nub [k | r <- slRows, (k, v) <- M.toList r, not (T.null v), k `notElem` ["surveyls_language", "surveyls_survey_id"]]
      slLang r = let l = get "surveyls_language" r in if T.null l then base else l
  forM_ (nub [local e | e <- children, local e `notElem` known, not (null (elements e)), not (M.member (local e) tables && null (table (local e)))]) $ \t ->
    warn "limesurvey" (t <> " dropped")
  forM_ built $ \(_, (scripts, _)) -> forM_ scripts $ \t -> warn t "question script dropped"
  forM_ defaults $ \(r, _) ->
    unless (T.null (get "specialtype" r)) $ warn "limesurvey" ("default value for " <> get "specialtype" r <> " option dropped")
  let orphans = [get "title" r | (gid, r) <- [(get "gid" r, r) | r <- mains], gid `notElem` gidOrder]
  unless (null orphans) $ warn "limesurvey" ("questions outside any group dropped: " <> T.intercalate ", " orphans)
  fromLs
    LsSurvey
      { lsId = get "sid" survey
      , lsLangs = langs
      , lsSettings = [(k, v) | (k, v) <- M.toList survey, not (T.null v), k `notElem` ["sid", "language", "additional_languages"]]
      , lsLangSettings = [(k, i18nFrom base [(slLang r, get k r) | r <- slRows]) | k <- slKeys]
      , lsGroups = groups
      }
  where
    orDefault d t = if T.null t then d else t
    known =
      [ "LimeSurveyDocType", "DBVersion", "languages", "answers", "answer_l10ns", "conditions", "defaultvalues", "defaultvalue_l10ns", "groups", "group_l10ns"
      , "questions", "subquestions", "question_l10ns", "question_attributes", "surveys", "surveys_languagesettings"
      ]

local :: X.Element -> Text
local = X.nameLocalName . X.elementName

elements :: X.Element -> [X.Element]
elements e = [c | X.NodeElement c <- X.elementNodes e]

content :: X.Element -> Text
content e = T.concat [t | X.NodeContent t <- X.elementNodes e]

tableRows :: X.Element -> [Row]
tableRows t = [M.fromList [(local c, content c) | c <- elements r] | rs <- elements t, local rs == "rows", r <- elements rs, local r == "row"]

------------------------------------------------------------------------------
-- Writing

-- | Attributes LimeSurvey stores per language.
i18nAttributes :: [Text]
i18nAttributes =
  [ "choice_title", "dropdown_prepostfix", "dualscale_headerA", "dualscale_headerB", "em_validation_q_tip", "em_validation_sq_tip"
  , "other_replace_text", "prefix", "rank_title", "suffix", "time_limit_countdown_message", "time_limit_message"
  , "time_limit_warning_2_message", "time_limit_warning_message"
  ]

writeLimeSurveyLss :: Form -> Conv BL.ByteString
writeLimeSurveyLss s = do
  ls <- toLs s
  let langs = lsLangs ls
      base = fromMaybe "en" (safeHead langs)
      sid = if T.null (lsId ls) then "123456" else lsId ls
      qs = [(lgId g, n, q) | g <- lsGroups ls, (n, q) <- zip [1 :: Int ..] (lgQuestions g)]
      inLangs t = [(l, i18nText (Just l) t) | l <- langs]
      questionRow gid parent order scale q =
        [ ("qid", lqId q), ("parent_qid", parent), ("sid", sid), ("gid", gid), ("type", lqType q), ("title", lqTitle q)
        , ("preg", lqPreg q), ("other", lqOther q), ("mandatory", lqMandatory q), ("encrypted", field "encrypted" "N" q)
        , ("question_order", tshow order), ("scale_id", tshow scale), ("same_default", field "same_default" "0" q)
        , ("relevance", lqRelevance q), ("question_theme_name", lqTheme q), ("modulename", field "modulename" "" q)
        , ("same_script", field "same_script" "0" q)
        ]
      field k d q = fromMaybe d (lookup k (lqFields q))
      subRow gid q (n, sq) = questionRow gid (lqId q) n (sqScale sq) ((lsQuestion (lqType q) (sqCode sq)) {lqId = sqId sq, lqRelevance = sqRelevance sq, lqMandatory = "", lqOther = "N"})
      answers = [(q, (n, a)) | (_, _, q) <- qs, (n, a) <- zip [1 :: Int ..] (lqAnswers q)]
      defaults = [(q, "0", d) | (_, _, q) <- qs, Just d <- [lqDefault q]] ++ [(q, sqId sq, d) | (_, _, q) <- qs, sq <- lqSubs q, Just d <- [sqDefault sq]]
      numbered xs = zip (map tshow [1 :: Int ..]) xs
      attrRows q (k, v)
        | k `elem` i18nAttributes || not (null (i18nLangs v)) = [[("qid", lqId q), ("attribute", k), ("value", t), ("language", l)] | (l, t) <- inLangs v]
        | otherwise = [[("qid", lqId q), ("attribute", k), ("value", i18nDefault v), ("language", "")]]
      tables =
        [ table "answers" ["aid", "qid", "code", "sortorder", "assessment_value", "scale_id"]
            [[("aid", aid), ("qid", lqId q), ("code", aCode a), ("sortorder", tshow n), ("assessment_value", orZero (aAssess a)), ("scale_id", tshow (aScale a))] | (aid, (q, (n, a))) <- numbered answers]
        , table "answer_l10ns" ["id", "aid", "answer", "language"]
            [[("id", i), ("aid", aid), ("answer", t), ("language", l)] | (i, (aid, l, t)) <- numbered [(aid, l, t) | (aid, (_, (_, a))) <- numbered answers, (l, t) <- inLangs (aText a)]]
        , table "defaultvalues" ["dvid", "qid", "scale_id", "sqid", "specialtype"]
            [[("dvid", dv), ("qid", lqId q), ("scale_id", "0"), ("sqid", sq), ("specialtype", "")] | (dv, (q, sq, _)) <- numbered defaults]
        , table "defaultvalue_l10ns" ["id", "dvid", "language", "defaultvalue"]
            [[("id", i), ("dvid", dv), ("language", l), ("defaultvalue", t)] | (i, (dv, l, t)) <- numbered [(dv, l, t) | (dv, (_, _, d)) <- numbered defaults, (l, t) <- inLangs d]]
        , table "groups" ["gid", "sid", "group_order", "randomization_group", "grelevance"]
            [[("gid", lgId g), ("sid", sid), ("group_order", tshow n), ("randomization_group", lgRandom g), ("grelevance", lgRelevance g)] | (n, g) <- zip [1 :: Int ..] (lsGroups ls)]
        , table "group_l10ns" ["id", "gid", "group_name", "description", "language"]
            [[("id", i), ("gid", lgId g), ("group_name", i18nText (Just l) (lgName g)), ("description", i18nText (Just l) (lgDesc g)), ("language", l)] | (i, (g, l)) <- numbered [(g, l) | g <- lsGroups ls, l <- langs]]
        , table "questions" questionFields [questionRow gid "0" n (0 :: Int) q | (gid, n, q) <- qs]
        , table "subquestions" questionFields [subRow gid q sq | (gid, _, q) <- qs, sq <- zip [1 :: Int ..] (lqSubs q)]
        , table "question_l10ns" ["id", "qid", "question", "help", "script", "language"]
            [ [("id", i), ("qid", qid), ("question", t), ("help", h), ("script", ""), ("language", l)]
            | (i, (qid, l, t, h)) <-
                numbered $
                  concat
                    [ [(lqId q, l, i18nText (Just l) (lqText q), i18nText (Just l) (lqHelp q)) | l <- langs]
                        ++ [(sqId sq, l, i18nText (Just l) (sqText sq), "") | sq <- lqSubs q, l <- langs]
                    | (_, _, q) <- qs
                    ]
            ]
        , table "question_attributes" ["qid", "attribute", "value", "language"] [r | (_, _, q) <- qs, a <- lqAttrs q, r <- attrRows q a]
        , table "surveys" (["sid", "language", "additional_languages"] ++ map fst (lsSettings ls))
            [[("sid", sid), ("language", base), ("additional_languages", T.unwords (drop 1 langs))] ++ lsSettings ls]
        , table "surveys_languagesettings" (["surveyls_survey_id", "surveyls_language"] ++ map fst (lsLangSettings ls))
            [[("surveyls_survey_id", sid), ("surveyls_language", l)] ++ [(k, i18nText (Just l) v) | (k, v) <- lsLangSettings ls] | l <- langs]
        ]
  when (any (not . null . lqConds) [q | (_, _, q) <- qs]) $ warn "limesurvey" "legacy conditions dropped"
  pure . BL.fromStrict . TE.encodeUtf8 . T.unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<document>"
    , " <LimeSurveyDocType>Survey</LimeSurveyDocType>"
    , " <DBVersion>" <> dbVersion <> "</DBVersion>"
    , " <languages>"
    ]
      ++ ["  <language>" <> escape l <> "</language>" | l <- langs]
      ++ [" </languages>"]
      ++ concat tables
      ++ ["</document>"]
  where
    orZero t = if T.null t then "0" else t

questionFields :: [Text]
questionFields =
  [ "qid", "parent_qid", "sid", "gid", "type", "title", "preg", "other", "mandatory", "encrypted", "question_order"
  , "scale_id", "same_default", "relevance", "question_theme_name", "modulename", "same_script"
  ]

-- | A table in LimeSurvey's layout; an empty table is left out, as the
-- exporter does.
table :: Text -> [Text] -> [[(Text, Text)]] -> [Text]
table _ _ [] = []
table name fields rows =
  [" <" <> name <> ">", "  <fields>"]
    ++ ["   <fieldname>" <> f <> "</fieldname>" | f <- fields]
    ++ ["  </fields>", "  <rows>"]
    ++ concat [["   <row>"] ++ mapMaybe (cell r) fields ++ ["   </row>"] | r <- rows]
    ++ ["  </rows>", " </" <> name <> ">"]
  where
    cell r f = case lookup f r of
      Nothing -> Nothing
      Just "" -> Just ("    <" <> f <> "/>")
      -- A parser turns a carriage return inside CDATA into a line feed, so
      -- it is written as a character reference between sections.
      Just v -> Just ("    <" <> f <> "><![CDATA[" <> T.replace "\r" "]]>&#13;<![CDATA[" (T.replace "]]>" "]]]]><![CDATA[>" v) <> "]]></" <> f <> ">")

-- | Values per key in their original order. (Prepending and reversing once
-- is linear; appending one at a time is quadratic in a key's row count.)
collect :: Ord k => [(k, [a])] -> M.Map k [a]
collect = M.map reverse . M.fromListWith (++)

escape :: Text -> Text
escape = T.replace "<" "&lt;" . T.replace "&" "&amp;"

safeHead :: [a] -> Maybe a
safeHead = \case
  (x : _) -> Just x
  [] -> Nothing

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack (T.strip t)) of
  [(n, "")] -> Just n
  _ -> Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show
