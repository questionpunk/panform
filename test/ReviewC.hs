-- | Regression tests for the pre-publication review of the XLSForm, XForm,
-- SurveyJS, FHIR, REDCap, SPSS, SurveyMonkey, Google Forms, GIFT, Aiken and
-- surveydown readers and writers.
module ReviewC (tests) where

import Control.Monad (when)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Formats
import Panform.Json (key, keyArr, keyText)
import Test.Tasty
import Test.Tasty.HUnit

------------------------------------------------------------------------------
-- Helpers

spec :: String -> FormatSpec
spec n = fromMaybe (error ("format missing: " <> n)) (lookupFormat n)

readAs :: String -> BL.ByteString -> IO (Form, [Warning])
readAs n bs = case fmtReader (spec n) of
  Nothing -> assertFailure (n <> " has no reader")
  Just r -> either (assertFailure . show) pure (runConv (r bs))

writeAs :: String -> Form -> IO (BL.ByteString, [Warning])
writeAs n s = case fmtWriter (spec n) of
  Nothing -> assertFailure (n <> " has no writer")
  Just w -> either (assertFailure . show) pure (runConv (w s))

text :: BL.ByteString -> Text
text = TE.decodeUtf8 . BL.toStrict

bytes :: Text -> BL.ByteString
bytes = BL.fromStrict . TE.encodeUtf8

json :: BL.ByteString -> IO Value
json = maybe (assertFailure "output is not JSON") pure . decode

-- | Write, read back, and write again: the questions come back the same
-- and the second write gives the same bytes. A reader may add what its
-- format always has (a form id, a page around loose questions).
exact :: String -> Form -> IO BL.ByteString
exact n s = do
  (out, _) <- writeAs n s
  (s', _) <- readAs n out
  questions s' @?= questions s
  (out2, _) <- writeAs n s'
  out2 @?= out
  pure out

-- | Read, write, read: the two reads have the same questions, and two
-- writes give the same bytes.
stable :: String -> BL.ByteString -> IO (Form, BL.ByteString)
stable n src = do
  (s, _) <- readAs n src
  (out, _) <- writeAs n s
  (s', _) <- readAs n out
  questions s' @?= questions s
  (out2, _) <- writeAs n s'
  out2 @?= out
  pure (s, out)

-- | As the corpus checks it: read, write, read back; unless the write or
-- the second read warned, the whole form (meta included) is the same.
noSilentChange :: String -> BL.ByteString -> IO [Warning]
noSilentChange n src = do
  (s, _) <- readAs n src
  (out, ws) <- writeAs n s
  (s', ws') <- readAs n out
  when (null ws && null ws') (s' @?= s)
  pure (ws ++ ws')

warned :: Text -> [Warning] -> Assertion
warned what ws = assertBool ("no warning about: " <> T.unpack what <> "\n got: " <> show ws) (any ((what `T.isInfixOf`) . warnWhat) ws)

has :: Text -> BL.ByteString -> Assertion
has needle out = assertBool (T.unpack needle <> " not in:\n" <> T.unpack (text out)) (needle `T.isInfixOf` text out)

hasNot :: Text -> BL.ByteString -> Assertion
hasNot needle out = assertBool (T.unpack needle <> " in:\n" <> T.unpack (text out)) (not (needle `T.isInfixOf` text out))

form :: [Item] -> Form
form = Form emptyMeta

one :: Question -> Form
one q = form [ItemQuestion q]

findQ :: Form -> Text -> Maybe Question
findQ s i = case [q | q <- questions s, qId q == i] of
  (q : _) -> Just q
  [] -> Nothing

ids :: Form -> [Text]
ids (Form _ is) = map itemId (allItems is)

-- | An XLSForm as pyxform's single-file CSV.
xls :: [Text] -> [Text] -> BL.ByteString
xls survey choices = bytes (T.unlines (survey ++ choices))

limit :: Maybe Double -> Maybe Double -> Maybe Validation
limit lo hi = Just (Validation lo hi Nothing Nothing)

self :: Expr
self = ERef "."

tests :: TestTree
tests =
  testGroup
    "review: xlsform, xform, surveyjs, fhir, redcap, spss and the text formats"
    [ lengthBounds
    , xlsformTests
    , xformTests
    , surveyjsTests
    , fhirTests
    , redcapTests
    , spssTests
    , smallFormats
    ]

------------------------------------------------------------------------------
-- A bound on a text question is a length

lengthBounds :: TestTree
lengthBounds =
  testGroup
    "a bound on a text answer is its length"
    [ testCase "xlsform writes string-length for text and a value range for a number" $ do
        out <- exact "xlsform-csv" (form [ItemQuestion name, ItemQuestion age])
        has "string-length(.) >= 2 and string-length(.) <= 80" out
        has ". >= 18 and . <= 99" out
    , testCase "xlsform reads a value comparison on text as an expression, not a length" $ do
        (s, _) <- stable "xlsform-csv" (xls ["survey,type,name,label,constraint", ",text,t,Code,. <= 80"] [])
        (findQ s "t" >>= qValidation) @?= Just (Validation Nothing Nothing (Just (EBin Le self (ELit (LNum 80)))) Nothing)
    , testCase "xlsform keeps bounds and an expression together" $ do
        let q = age {qValidation = Just (Validation (Just 18) (Just 99) (Just (EBin Neq self (ELit (LNum 50)))) Nothing)}
        out <- exact "xlsform-csv" (one q)
        has ". >= 18 and . <= 99 and . != 50" out
    , testCase "xform writes string-length for text" $ do
        out <- exact "xform" (form [ItemQuestion name, ItemQuestion age])
        has "constraint=\"string-length(.) &gt;= 2 and string-length(.) &lt;= 80\"" out
        has "constraint=\". &gt;= 18 and . &lt;= 99\"" out
    , testCase "xform keeps bounds and an expression together" $
        () <$ exact "xform" (one age {qValidation = Just (Validation (Just 18) Nothing (Just (EBin Neq self (ELit (LNum 50)))) Nothing)})
    , testCase "surveyjs writes a text validator for text and a numeric one for a number" $ do
        out <- exact "surveyjs" (form [ItemQuestion name, ItemQuestion age])
        v <- json out
        let validators = [(keyText "type" x, keyText "maxLength" x, keyText "maxValue" x) | p <- keyArr "pages" v, e <- keyArr "elements" p, x <- keyArr "validators" e]
        validators @?= [(Just "text", Just "80", Nothing), (Just "numeric", Nothing, Just "99")]
    , testCase "surveyjs keeps a numeric check on a text answer as it is" $ do
        (s, out) <- stable "surveyjs" "{\"elements\":[{\"type\":\"text\",\"name\":\"t\",\"validators\":[{\"type\":\"numeric\",\"minValue\":3}]}]}"
        (findQ s "t" >>= qValidation) @?= Nothing
        has "\"minValue\": 3" out
    , testCase "redcap says a length limit has no place" $ do
        (out, ws) <- writeAs "redcap" (one name)
        warned "text length limit dropped" ws
        hasNot ",2,80," out
    , testCase "fhir reads maxLength as the upper bound and writes it back" $ do
        (s, out) <- stable "fhir" "{\"resourceType\":\"Questionnaire\",\"status\":\"draft\",\"item\":[{\"linkId\":\"t\",\"text\":\"Name\",\"type\":\"string\",\"maxLength\":80}]}"
        (findQ s "t" >>= qValidation) @?= limit Nothing (Just 80)
        has "\"maxLength\": 80" out
    ]
  where
    name = (question "name" "Name" (QText ShortText)) {qValidation = limit (Just 2) (Just 80)}
    age = (question "age" "Age" (QNumber Integer)) {qValidation = limit (Just 18) (Just 99)}

------------------------------------------------------------------------------
-- XLSForm

xlsformTests :: TestTree
xlsformTests =
  testGroup
    "xlsform"
    [ testCase "a stray end marker does not end the survey" $ do
        (s, ws) <- readAs "xlsform-csv" (xls ["survey,type,name,label", ",text,a,A", ",end_group,,", ",text,b,B", ",text,c,C"] [])
        ids s @?= ["a", "b", "c"]
        warned "without a matching begin" ws
    , testCase "a mismatched or missing end marker is reported" $ do
        (s, ws) <- readAs "xlsform-csv" (xls ["survey,type,name,label", ",begin_group,g,G", ",text,a,A", ",end_repeat,,", ",begin_repeat,r,R", ",text,b,B"] [])
        ids s @?= ["g", "a", "r", "b"]
        warned "begin group closed by end repeat" ws
        warned "never closed" ws
    , testCase "a repeat of list-nolabel selects stays a repeat" $ do
        (s, _) <- stable "xlsform-csv" (xls ["survey,type,name,label,appearance", ",begin_repeat,r,Each,field-list", ",select_one yn,r_a,A,list-nolabel", ",end_repeat,,,"] yn)
        [groupKind g | ItemGroup g <- formItems s] @?= [Repeat]
    , testCase "table rows with logic or differing required flags stay questions" $ do
        let table extra = xls (["survey,type,name,label,appearance,required,relevant", ",begin_group,g,Rate,field-list,,"] ++ extra ++ [",end_group,,,,,"]) yn
        (s1, _) <- stable "xlsform-csv" (table [",select_one yn,g_a,A,list-nolabel,,${x} = 1", ",select_one yn,g_b,B,list-nolabel,,"])
        (qRelevant =<< findQ s1 "g_a") @?= Just (EBin Eq (ERef "x") (ELit (LNum 1)))
        (s2, _) <- stable "xlsform-csv" (table [",select_one yn,g_a,A,list-nolabel,yes,", ",select_one yn,g_b,B,list-nolabel,,"])
        map (fmap qRequired . findQ s2) ["g_a", "g_b"] @?= [Just True, Just False]
        -- row names that are not <group>_<row> would be renamed on writing
        (s3, _) <- stable "xlsform-csv" (table [",select_one yn,first,A,list-nolabel,,", ",select_one yn,second,B,list-nolabel,,"])
        ids s3 @?= ["g", "first", "second"]
    , testCase "the writer's own matrix still reads as a matrix" $ do
        let m = QMatrix Single [choice "a" "A", choice "b" "B"] [choice "1" "Bad", choice "2" "Good"]
        (out, _) <- writeAs "xlsform-csv" (one (question "g" "Rate" m) {qRequired = True})
        (s, _) <- readAs "xlsform-csv" out
        fmap (\q -> (qType q, qRequired q)) (findQ s "g") @?= Just (m, True)
        (out2, _) <- writeAs "xlsform-csv" s
        out2 @?= out
    , testCase "a required expression is kept" $ do
        (s, out) <- stable "xlsform-csv" (xls ["survey,type,name,label,required", ",text,a,A,${age} >= 18", ",text,b,B,yes"] [])
        fmap qRequired (findQ s "a") @?= Just False
        (M.lookup "xlsform:required" . qAttrs =<< findQ s "a") @?= Just (String "${age} >= 18")
        has ",text,a,A,${age} >= 18" out
    , testCase "a language used only in a hint or a message gets its column" $ do
        let q = (question "a" "A" (QNumber Integer)) {qHint = Just (I18n "Years" (M.fromList [("fr", "Ans")])), qValidation = Just (Validation (Just 1) Nothing Nothing (Just (I18n "Too low" (M.fromList [("de", "Zu klein")]))))}
        out <- exact "xlsform-csv" (one q)
        has "hint::fr" out
        has "constraint_message::de" out
    , testCase "extra columns of the choices sheet survive, with their spelling" $ do
        let src =
              xls
                ["survey,type,name,label,choice_filter", ",select_one states,state,State,", ",select_one cities,city,City,State=${state}"]
                ["choices,list_name,name,label,State,media::image", ",states,ny,New York,,ny.png", ",cities,nyc,New York City,ny,", ",cities,alb,Albany,ny,"]
        (_, out) <- stable "xlsform-csv" src
        has "list_name,name,label,State,media::image" out
        has ",cities,nyc,New York City,ny," out
        has ",states,ny,New York,,ny.png" out
    , testCase "columns of a choice list no question uses are not kept silently" $ do
        let src =
              xls
                ["survey,type,name,label", ",select_one yn,q,Q"]
                ["choices,list_name,name,label,image", ",yn,yes,Yes,y.png", ",yn,no,No,", ",faces,happy,Happy,happy.png", ",faces,sad,Sad,sad.png"]
        (_, ws) <- readAs "xlsform-csv" src
        warned "choice lists no question uses dropped: faces" ws
        _ <- noSilentChange "xlsform-csv" src
        (_, out) <- stable "xlsform-csv" src
        has ",yn,yes,Yes,y.png" out
    , testCase "columns kept for a list the writer merges into another are reported" $ do
        let src =
              xls
                ["survey,type,name,label", ",select_one a,q1,Q1", ",select_one b,q2,Q2"]
                ["choices,list_name,name,label,image", ",a,x,X,a.png", ",b,x,X,b.png"]
        (s, _) <- readAs "xlsform-csv" src
        (_, ws) <- writeAs "xlsform-csv" s
        warned "choices sheet columns dropped for lists: b" ws
    , testCase "a media column is not a language" $ do
        (s, _) <- readAs "xlsform-csv" (xls ["survey,type,name,label::English (en),media::image", ",text,a,A,a.png"] ["settings,default_language", ",English (en)"])
        metaLanguages (formMeta s) @?= ["en"]
    , testCase "parameters are kept, and a range keeps its step" $ do
        (_, out) <- stable "xlsform-csv" (xls ["survey,type,name,label,parameters", ",select_one yn,a,A,randomize=true", ",range,r,R,start=0 end=10 step=5"] yn)
        has "randomize=true" out
        has "start=0 end=10 step=5" out
    , testCase "a note's other columns are named when dropped" $ do
        (_, ws) <- readAs "xlsform-csv" (xls ["survey,type,name,label,hint,appearance", ",note,n,Read this,Carefully,bold"] [])
        warned "note columns dropped: appearance, hint" ws
    , testCase "spaces in the names of a multiple choice are reported" $ do
        (_, ws) <- writeAs "xlsform-csv" (one (question "q" "Q" (QChoice Multiple Radio [choice "New York" "New York", choice "b" "B"])))
        warned "choice names contain spaces" ws
    , testCase "a long choice list keeps its order" $ do
        let n = 3000 :: Int
            rows = [",big," <> T.pack (show i) <> ",Option " <> T.pack (show i) | i <- [1 .. n]]
        (s, _) <- readAs "xlsform-csv" (xls ["survey,type,name,label", ",select_one big,q,Q"] ("choices,list_name,name,label" : rows))
        fmap qType (findQ s "q") @?= Just (QChoice Single Radio [choice (T.pack (show i)) ("Option " <> T.pack (show i)) | i <- [1 .. n]])
    ]
  where
    yn = ["choices,list_name,name,label", ",yn,yes,Yes", ",yn,no,No"]

------------------------------------------------------------------------------
-- XForm

xformTests :: TestTree
xformTests =
  testGroup
    "xform"
    [ testCase "a required expression is kept" $ do
        (s, out) <- stable "xform" (xform "<bind nodeset=\"/data/a\" type=\"string\" required=\"/data/age &gt;= 18\"/>")
        fmap qRequired (findQ s "a") @?= Just False
        has "required=\"/data/age &gt;= 18\"" out
    , testCase "a required expression crosses between xlsform and xform" $ do
        (s, _) <- readAs "xlsform-csv" (xls ["survey,type,name,label,required", ",integer,age,Age,", ",text,a,A,${age} >= 18"] [])
        (out, _) <- writeAs "xform" s
        has "required=\"/data/age &gt;= 18\"" out
        (s2, _) <- readAs "xform" (xform "<bind nodeset=\"/data/a\" type=\"string\" required=\"/data/age &gt;= 18\"/>")
        (_, ws) <- writeAs "xlsform-csv" s2
        warned "required expression /data/age >= 18 dropped" ws
    , testCase "a default with spaces around it is trimmed with a warning" $ do
        (out, ws) <- writeAs "xform" (one (question "a" "A" (QText ShortText)) {qDefault = Just "  value  "})
        warned "spaces around the default value trimmed" ws
        has "<a>value</a>" out
        (out2, ws2) <- writeAs "xform" (one (question "a" "A" (QText ShortText)) {qDefault = Just "   "})
        warned "blank default value dropped" ws2
        has "<a/>" out2
    , testCase "logic on an id two items share is reported" $ do
        let x = question "x" "X" (QText ShortText)
            g n = ItemGroup (Group n Section Nothing Nothing [ItemQuestion x] M.empty)
            last' = (question "z" "Z" (QText ShortText)) {qRelevant = Just (EBin Eq (ERef "x") (ELit (LStr "a")))}
        (_, ws) <- writeAs "xform" (form [g "g1", g "g2", ItemQuestion last'])
        warned "logic refers to x, an id several items share" ws
    , testCase "table rows with a hint or differing required flags stay questions" $ do
        let row n req = (question n n (QChoice Single Radio [choice "1" "Bad", choice "2" "Good"])) {qRequired = req, qAttrs = M.fromList [("xform:appearance", String "list-nolabel")]}
            grp = form [ItemGroup (Group "g" Page (Just (plain "Rate")) Nothing [ItemQuestion (row "g_a" True), ItemQuestion (row "g_b" False)] M.empty)]
        (out, _) <- writeAs "xform" grp
        (s, _) <- readAs "xform" out
        map (fmap qRequired . findQ s) ["g_a", "g_b"] @?= [Just True, Just False]
    ]
  where
    xform bind =
      bytes . T.concat $
        [ "<?xml version=\"1.0\"?><h:html xmlns=\"http://www.w3.org/2002/xforms\" xmlns:h=\"http://www.w3.org/1999/xhtml\">"
        , "<h:head><h:title>T</h:title><model><instance><data id=\"t\"><age/><a/></data></instance>"
        , "<bind nodeset=\"/data/age\" type=\"int\"/>"
        , bind
        , "</model></h:head><h:body><input ref=\"/data/age\"><label>Age</label></input><input ref=\"/data/a\"><label>A</label></input></h:body></h:html>"
        ]

------------------------------------------------------------------------------
-- SurveyJS

surveyjsTests :: TestTree
surveyjsTests =
  testGroup
    "surveyjs"
    [ testCase "bounds and an expression are two validators and both read back" $
        () <$ exact "surveyjs" (one (question "n" "N" (QNumber Decimal)) {qValidation = Just (Validation (Just 1) (Just 5) (Just (EBin Neq (ERef "n") (ELit (LNum 3)))) (Just (plain "No")))})
    , testCase "validators the IR cannot hold are kept" $ do
        (s, out) <- stable "surveyjs" "{\"elements\":[{\"type\":\"text\",\"name\":\"t\",\"validators\":[{\"type\":\"regex\",\"regex\":\"^a\"},{\"type\":\"text\",\"maxLength\":9},{\"type\":\"email\"}]}]}"
        (findQ s "t" >>= qValidation) @?= limit Nothing (Just 9)
        has "\"regex\": \"^a\"" out
        has "\"type\": \"email\"" out
    , testCase "properties the IR has no field for are kept" $ do
        let src =
              "{\"triggers\":[{\"type\":\"complete\",\"expression\":\"{a} = 1\"}],\"calculatedValues\":[{\"name\":\"v\",\"expression\":\"1\"}],\"pages\":[{\"name\":\"p\",\"navigationTitle\":\"One\",\"elements\":["
                <> "{\"type\":\"text\",\"name\":\"a\",\"enableIf\":\"{b} = 1\",\"requiredIf\":\"{b} = 2\",\"setValueIf\":\"{b} = 3\",\"defaultValue\":5},"
                <> "{\"type\":\"dropdown\",\"name\":\"b\",\"choicesByUrl\":{\"url\":\"https://example.org/c\"},\"choices\":[{\"value\":1,\"text\":\"One\",\"visibleIf\":\"{a} = 1\"},\"two\"]},"
                <> "{\"type\":\"checkbox\",\"name\":\"c\",\"choices\":[\"x\",\"y\"],\"defaultValue\":[\"x\",\"y\"]},"
                <> "{\"type\":\"panel\",\"name\":\"pn\",\"state\":\"collapsed\",\"elements\":[{\"type\":\"boolean\",\"name\":\"d\",\"defaultValue\":true}]}"
                <> "]}]}"
        (s, out) <- stable "surveyjs" src
        fmap qDefault (findQ s "a") @?= Just (Just "5")
        fmap qType (findQ s "b") @?= Just (QChoice Single Dropdown [choice "1" "One", choice "two" "two"])
        v <- json out
        orig <- json src
        -- nothing of the source is missing from the output
        assertBool ("output lacks part of the source:\n" <> T.unpack (text out)) (orig `within` v)
    , testCase "a type the writer would not choose is written back" $ do
        let src = "{\"elements\":[{\"type\":\"signaturepad\",\"name\":\"s\"},{\"type\":\"imagepicker\",\"name\":\"i\",\"multiSelect\":true,\"choices\":[{\"value\":\"a\",\"imageLink\":\"a.png\"}]},{\"type\":\"rating\",\"name\":\"r\",\"rateValues\":[1,2,3]},{\"type\":\"text\",\"name\":\"c\",\"inputType\":\"color\"},{\"type\":\"text\",\"name\":\"h\",\"visible\":false,\"inputType\":\"number\"}]}"
        (_, out) <- stable "surveyjs" src
        v <- json out
        orig <- json src
        assertBool ("output lacks part of the source:\n" <> T.unpack (text out)) (orig `within` v)
    , testCase "a property that names a choice list elsewhere is kept on a question without choices" $ do
        let src = "{\"pages\":[{\"name\":\"p\",\"elements\":[{\"type\":\"comment\",\"name\":\"c\",\"rows\":3},{\"type\":\"boolean\",\"name\":\"b\",\"inputType\":\"x\"}]}]}"
        ws <- noSilentChange "surveyjs" src
        ws @?= []
        (_, out) <- stable "surveyjs" src
        has "\"rows\": 3" out
        has "\"inputType\": \"x\"" out
    , testCase "an html element's other properties are named when dropped" $ do
        (_, ws) <- readAs "surveyjs" "{\"elements\":[{\"type\":\"html\",\"name\":\"h\",\"html\":\"<b>hi</b>\",\"enableIf\":\"{a} = 1\"}]}"
        warned "html element properties dropped: enableIf" ws
    ]

-- | Every field of the first value is in the second. The writer may add
-- defaults (a title, a page around loose elements) and spell a number as
-- it was read.
within :: Value -> Value -> Bool
within a b = case (a, b) of
  (Object o, Object _)
    | Just es <- KM.lookup "elements" o, Nothing <- KM.lookup "pages" o, [p] <- keyArr "pages" b ->
        Object (KM.delete "elements" o) `within` b && es `within` fromMaybe Null (key "elements" p)
    | otherwise -> and [maybe False (x `within`) (KM.lookup k' bo) | (k', x) <- KM.toList o, Object bo <- [b]]
  (Array xs, Array ys) -> length xs == length ys && and (zipWith within (toList xs) (toList ys))
  _ -> a == b

------------------------------------------------------------------------------
-- FHIR

fhirTests :: TestTree
fhirTests =
  testGroup
    "fhir"
    [ testCase "a matrix with a hint keeps its rows and its hint" $ do
        let q = (question "m" "Rate" (QMatrix Single [choice "a" "Speed", choice "b" "Price"] [choice "1" "Bad", choice "2" "Good"])) {qHint = Just (plain "One per row")}
        out <- exact "fhir" (one q)
        v <- json out
        [map (keyText "linkId") (keyArr "item" i) | i <- keyArr "item" v] @?= [[Just "m_hint", Just "m_a", Just "m_b"]]
    , testCase "a table whose rows differ in required stays a group" $ do
        let row i req = "{\"linkId\":\"" <> i <> "\",\"text\":\"R\",\"type\":\"choice\"" <> req <> ",\"answerOption\":[{\"valueCoding\":{\"code\":\"1\"}}]}"
        (s, _) <- stable "fhir" (questionnaire ("{\"linkId\":\"g\",\"type\":\"group\",\"extension\":[" <> control "table" <> "],\"item\":[" <> row "g_a" ",\"required\":true" <> "," <> row "g_b" "" <> "]}"))
        map (fmap qRequired . findQ s) ["g_a", "g_b"] @?= [Just True, Just False]
    , testCase "matrix rows keep ids that are not <matrix>_<row>" $ do
        let row i = "{\"linkId\":\"" <> i <> "\",\"text\":\"R\",\"type\":\"choice\",\"answerOption\":[{\"valueCoding\":{\"code\":\"1\"}}]}"
        (s, out) <- stable "fhir" (questionnaire ("{\"linkId\":\"g\",\"type\":\"group\",\"extension\":[" <> control "table" <> "],\"item\":[" <> row "g.1" <> "," <> row "other" <> "]}"))
        [() | Just (QMatrix {}) <- [qType <$> findQ s "g"]] @?= [()]
        has "\"linkId\": \"g.1\"" out
        has "\"linkId\": \"other\"" out
    , testCase "several initial answers are kept and reported" $ do
        (s, ws) <- readAs "fhir" (questionnaire "{\"linkId\":\"q\",\"type\":\"string\",\"repeats\":true,\"initial\":[{\"valueString\":\"red\"},{\"valueString\":\"blue\"}]}")
        warned "several initial answers kept only for FHIR" ws
        (out, _) <- writeAs "fhir" s
        has "\"valueString\": \"blue\"" out
    , testCase "an initially selected option with a space in its code stays selected" $ do
        let opt c sel = "{\"valueCoding\":{\"code\":\"" <> c <> "\"}" <> sel <> "}"
            item opts = questionnaire ("{\"linkId\":\"q\",\"type\":\"choice\",\"repeats\":true,\"answerOption\":[" <> T.intercalate "," opts <> "]}")
        (s, out) <- stable "fhir" (item [opt "New York" ",\"initialSelected\":true", opt "Paris" ""])
        (findQ s "q" >>= qDefault) @?= Just "New York"
        has "\"initialSelected\": true" out
        hasNot "\"initial\":" out
        (_, out2) <- stable "fhir" (item [opt "New York" ",\"initialSelected\":true", opt "Paris" ",\"initialSelected\":true", opt "Rome" ""])
        length (T.breakOnAll "\"initialSelected\": true" (text out2)) @?= 2
    , testCase "a contained value set and the reference to it are kept" $ do
        let src =
              "{\"resourceType\":\"Questionnaire\",\"status\":\"draft\",\"contained\":[{\"resourceType\":\"ValueSet\",\"id\":\"vs\",\"name\":\"Colours\",\"status\":\"active\","
                <> "\"compose\":{\"include\":[{\"system\":\"http://example.org/a\",\"concept\":[{\"code\":\"r\",\"display\":\"Red\"}]},{\"system\":\"http://example.org/b\",\"concept\":[{\"code\":\"g\",\"display\":\"Green\"}]}]}}],"
                <> "\"item\":[{\"linkId\":\"q\",\"text\":\"Colour\",\"type\":\"choice\",\"answerValueSet\":\"#vs\",\"initial\":[{\"valueCoding\":{\"code\":\"g\"}}]}]}"
        (s, out) <- stable "fhir" (bytes src)
        fmap qType (findQ s "q") @?= Just (QChoice Single Radio [choice "r" "Red", choice "g" "Green"])
        v <- json out
        orig <- json (bytes src)
        v @?= orig
    , testCase "a value set that cannot be listed in full is reported" $ do
        let src =
              "{\"resourceType\":\"Questionnaire\",\"status\":\"draft\",\"contained\":[{\"resourceType\":\"ValueSet\",\"id\":\"vs\","
                <> "\"compose\":{\"include\":[{\"system\":\"http://example.org/a\",\"concept\":[{\"code\":\"r\"}]}],\"exclude\":[{\"system\":\"http://example.org/a\",\"concept\":[{\"code\":\"r\"}]}]}}],"
                <> "\"item\":[{\"linkId\":\"q\",\"type\":\"choice\",\"answerValueSet\":\"#vs\"}]}"
        (_, ws) <- readAs "fhir" (bytes src)
        warned "may be incomplete or ambiguous: exclusions" ws
    , testCase "options with more than a code and a label are written back as they were" $ do
        let src =
              questionnaire $
                "{\"linkId\":\"q\",\"text\":\"Q\",\"type\":\"choice\",\"answerOption\":["
                  <> "{\"valueCoding\":{\"system\":\"http://example.org/a\",\"code\":\"1\",\"display\":\"One\"},\"extension\":[{\"url\":\"http://hl7.org/fhir/StructureDefinition/ordinalValue\",\"valueDecimal\":1}]},"
                  <> "{\"valueCoding\":{\"system\":\"http://example.org/b\",\"code\":\"1\",\"display\":\"Uno\"}}]}"
        (_, out) <- stable "fhir" src
        has "ordinalValue" out
        has "http://example.org/b" out
    , testCase "answer options on a number are kept and reported" $ do
        (s, ws) <- readAs "fhir" (questionnaire "{\"linkId\":\"q\",\"type\":\"integer\",\"answerOption\":[{\"valueInteger\":1},{\"valueInteger\":2}]}")
        warned "answer options kept only for FHIR" ws
        (out, _) <- writeAs "fhir" s
        has "\"valueInteger\": 2" out
    , testCase "a help item keeps its id and control, and _text keeps its extensions" $ do
        let src =
              questionnaire $
                "{\"linkId\":\"q\",\"text\":\"Q\",\"_text\":{\"extension\":[{\"url\":\"http://hl7.org/fhir/StructureDefinition/rendering-style\",\"valueString\":\"color:red\"}]},\"type\":\"string\","
                  <> "\"item\":[{\"linkId\":\"q-help\",\"text\":\"Help\",\"type\":\"display\",\"extension\":[" <> control "flyover" <> "]}]}"
        (s, out) <- stable "fhir" src
        (findQ s "q" >>= qHint) @?= Just (plain "Help")
        v <- json out
        orig <- json src
        v @?= orig
    , testCase "an expression keeps its name and description, a constraint its key" $ do
        let expr = "{\"name\":\"score\",\"description\":\"Total\",\"language\":\"text/fhirpath\",\"expression\":\"1 + 1\"}"
            src =
              questionnaire $
                "{\"linkId\":\"q\",\"text\":\"Q\",\"type\":\"integer\",\"readOnly\":true,\"extension\":["
                  <> "{\"url\":\"http://hl7.org/fhir/uv/sdc/StructureDefinition/sdc-questionnaire-calculatedExpression\",\"valueExpression\":" <> expr <> "}]},"
                  <> "{\"linkId\":\"r\",\"text\":\"R\",\"type\":\"integer\",\"extension\":[{\"url\":\"http://hl7.org/fhir/StructureDefinition/targetConstraint\",\"extension\":["
                  <> "{\"url\":\"key\",\"valueId\":\"c9\"},{\"url\":\"severity\",\"valueCode\":\"warning\"},{\"url\":\"expression\",\"valueExpression\":{\"language\":\"text/fhirpath\",\"expression\":\"$this > 0\"}},{\"url\":\"human\",\"valueString\":\"Positive\"}]}]}"
        (_, out) <- stable "fhir" src
        has "\"description\": \"Total\"" out
        has "\"valueId\": \"c9\"" out
        has "\"valueCode\": \"warning\"" out
    , testCase "a display item's other fields are named when dropped" $ do
        (_, ws) <- readAs "fhir" (questionnaire ("{\"linkId\":\"d\",\"text\":\"Note\",\"type\":\"display\",\"readOnly\":true,\"extension\":[" <> control "inline" <> "]}"))
        warned "display item fields dropped: readOnly" ws
    ]
  where
    questionnaire item = bytes ("{\"resourceType\":\"Questionnaire\",\"status\":\"draft\",\"item\":[" <> item <> "]}")
    control c = "{\"url\":\"http://hl7.org/fhir/StructureDefinition/questionnaire-itemControl\",\"valueCodeableConcept\":{\"coding\":[{\"system\":\"http://hl7.org/fhir/questionnaire-item-control\",\"code\":\"" <> c <> "\"}]}}"

------------------------------------------------------------------------------
-- REDCap

redcapTests :: TestTree
redcapTests =
  testGroup
    "redcap"
    [ testCase "a record_id later in the survey is moved, not duplicated" $ do
        (out, ws) <- writeAs "redcap" (form [ItemQuestion (question "age" "Age" (QNumber Integer)), ItemQuestion (question "record_id" "Record" (QText ShortText))])
        map (T.takeWhile (/= ',')) (drop 1 (T.lines (text out))) @?= ["record_id", "age"]
        warned "moved to the first field" ws
    , testCase "a hidden field keeps its annotation and validation" $ do
        (s, out) <- stable "redcap" (dict ["secret,form_1,,text,Secret,,,integer,1,9,,,,,,,,@HIDDEN-SURVEY @DEFAULT='x'"])
        fmap qType (findQ s "secret") @?= Just QHidden
        has "integer,1,9" out
        has "@HIDDEN-SURVEY @DEFAULT='x'" out
    , testCase "a date range is kept" $ do
        (_, out) <- stable "redcap" (dict ["dob,form_1,,text,Born,,,date_ymd,1900-01-01,2030-12-31,,,,,,,,"])
        has "date_ymd,1900-01-01,2030-12-31" out
    , testCase "a slider keeps its middle label and whether it shows its number" $ do
        (s, out) <- stable "redcap" (dict ["pain,form_1,,slider,Pain,None | Some | Worst,,,,,,,,,,,,", "mood,form_1,,slider,Mood,Low,,number,,,,,,,,,,"])
        fmap qType (findQ s "pain") @?= Just (QScale 0 100 (Just (plain "None")) (Just (plain "Worst")))
        fmap qType (findQ s "mood") @?= Just (QScale 0 100 (Just (plain "Low")) Nothing)
        has "None | Some | Worst,,,0,100" out
        has "Low |  | ,,number,0,100" out
    , testCase "a question number and a matrix ranking flag are written back" $ do
        (_, out) <- stable "redcap" (dict ["a,form_1,,text,A,,,,,,,,,,7b,,,", "m_1,form_1,,radio,One,\"1, x | 2, y\",,,,,,,,,,m,y,", "m_2,form_1,,radio,Two,\"1, x | 2, y\",,,,,,,,,,m,y,"])
        has ",7b," out
        has ",m,y," out
    , testCase "matrix rows that differ in more than their label stay questions in their group" $ do
        (s, out) <- stable "redcap" (dict ["m_1,form_1,,radio,One,\"1, x | 2, y\",Note one,,,,,,y,,,m,,", "m_2,form_1,,radio,Two,\"1, x | 2, y\",,,,,,[m_1] = '1',,,,m,,"])
        map (fmap qRequired . findQ s) ["m_1", "m_2"] @?= [Just True, Just False]
        has "Note one" out
        has "[m_1] = '1'" out
        length (T.breakOnAll ",m,," (text out)) @?= 2
    , testCase "a section title over a field with its own header is reported" $ do
        let inner = ItemGroup (Group "inner" Section (Just (plain "Inner")) Nothing [ItemQuestion (question "a" "A" (QText ShortText))] M.empty)
        (out, ws) <- writeAs "redcap" (form [ItemGroup (Group "outer" Section (Just (plain "Outer")) Nothing [inner] M.empty)])
        warned "written as one section header" ws
        has "Outer Inner" out
    , testCase "a repeat is reported" $ do
        (_, ws) <- writeAs "redcap" (form [ItemGroup (Group "kids" Repeat (Just (plain "Each child")) Nothing [ItemQuestion (question "a" "A" (QText ShortText))] M.empty)])
        warned "repeat written as a section" ws
    ]
  where
    dict rows =
      bytes . T.unlines $
        "\"Variable / Field Name\",\"Form Name\",\"Section Header\",\"Field Type\",\"Field Label\",\"Choices, Calculations, OR Slider Labels\",\"Field Note\",\"Text Validation Type OR Show Slider Number\",\"Text Validation Min\",\"Text Validation Max\",Identifier?,\"Branching Logic (Show field only if...)\",\"Required Field?\",\"Custom Alignment\",\"Question Number (surveys only)\",\"Matrix Group Name\",\"Matrix Ranking?\",\"Field Annotation\""
          : "record_id,form_1,,text,Record ID,,,,,,,,,,,,,"
          : rows

------------------------------------------------------------------------------
-- SPSS

spssTests :: TestTree
spssTests =
  testGroup
    "spss"
    [ testCase "an apostrophe inside a double-quoted label does not swallow the next command" $ do
        (s, _) <- readAs "spss" (bytes (T.unlines ["NUMERIC q1 (F1.0).", "NUMERIC q2 (F1.0).", "VALUE LABELS q1 1 \"Don't know\".", "VARIABLE LABELS q2 'Second'."]))
        fmap qType (findQ s "q1") @?= Just (QChoice Single Radio [choice "1" "Don't know"])
        fmap (i18nDefault . qLabel) (findQ s "q2") @?= Just "Second"
    , testCase "a double quote in a label survives the writer's own output" $ do
        let q1 = question "q1" "A 5\" screen" (QChoice Single Radio [choice "1" "It's fine", choice "2" "Too \"small\""])
            q2 = question "q2" "Don't /* not a comment */ stop." (QNumber Integer)
        (out, _) <- writeAs "spss" (form [ItemQuestion q1, ItemQuestion q2])
        (s, _) <- readAs "spss" out
        map (i18nDefault . qLabel) (questions s) @?= ["A 5\" screen", "Don't /* not a comment */ stop."]
        fmap qType (findQ s "q1") @?= Just (QChoice Single Radio [choice "1" "It's fine", choice "2" "Too \"small\""])
    , testCase "a string left open on one line closes on the next" $ do
        (s, _) <- readAs "spss" (bytes (T.unlines ["NUMERIC a (F1.0).", "NUMERIC b (F1.0).", "VARIABLE LABELS a 'one.", "more'.", "VARIABLE LABELS b 'Second'."]))
        fmap (i18nDefault . qLabel) (findQ s "b") @?= Just "Second"
    , testCase "a comment in /* */ is dropped only outside a string" $ do
        (s, _) <- readAs "spss" (bytes (T.unlines ["NUMERIC a (F1.0). /* note 'x */", "VARIABLE LABELS a \"keep /* this */\"."]))
        fmap (i18nDefault . qLabel) (findQ s "a") @?= Just "keep /* this */"
    , testCase "the example dictionary reads as it did" $ do
        src <- BL.readFile "examples/openstatspec-dictionary.sps"
        (s, ws) <- readAs "spss" src
        (length (questions s), length ws) @?= (16, 12)
        sum [length cs | q <- questions s, QChoice _ _ cs <- [qType q]] @?= 17
    , testCase "names differing only in case are one variable, in first-seen order" $ do
        (s, _) <- readAs "spss" (bytes (T.unlines ["NUMERIC Beta (F1.0).", "NUMERIC alpha (F1.0).", "VARIABLE LABELS BETA 'B' ALPHA 'A'."]))
        map (\q -> (qId q, i18nDefault (qLabel q))) (questions s) @?= [("Beta", "B"), ("alpha", "A")]
    ]

------------------------------------------------------------------------------
-- SurveyMonkey, Google Forms, GIFT, Aiken, surveydown

smallFormats :: TestTree
smallFormats =
  testGroup
    "surveymonkey, google forms, gift, aiken, surveydown"
    [ testCase "a surveymonkey demographic question keeps its subtype and rows" $ do
        let src = "{\"pages\":[{\"questions\":[{\"id\":\"1\",\"family\":\"demographic\",\"subtype\":\"us\",\"headings\":[{\"heading\":\"Address\"}],\"answers\":{\"rows\":[{\"type\":\"name\",\"text\":\"Name\",\"visible\":true}]}}]}]}"
        (s, _) <- readAs "surveymonkey" src
        (out, _) <- writeAs "surveymonkey" s
        has "\"subtype\": \"us\"" out
        has "\"text\": \"Name\"" out
    , testCase "a google forms update changes only the masked fields" $ do
        let create = "{\"createItem\":{\"location\":{\"index\":0},\"item\":{\"title\":\"Colour?\",\"questionItem\":{\"question\":{\"questionId\":\"q1\",\"choiceQuestion\":{\"type\":\"RADIO\",\"options\":[{\"value\":\"Red\"}]}}}}}}"
            update = "{\"updateItem\":{\"location\":{\"index\":0},\"updateMask\":\"questionItem.question.required\",\"item\":{\"questionItem\":{\"question\":{\"required\":true}}}}}"
            info = "{\"updateFormInfo\":{\"info\":{\"title\":\"T\",\"description\":\"D\"},\"updateMask\":\"title,description\"}},{\"updateFormInfo\":{\"info\":{\"title\":\"ignored\"},\"updateMask\":\"description\"}}"
            missing = "{\"updateItem\":{\"location\":{\"index\":7},\"updateMask\":\"title\",\"item\":{\"title\":\"x\"}}}"
        (s, ws) <- readAs "google-forms-batch" (bytes ("{\"requests\":[" <> T.intercalate "," [info, create, update, missing] <> "]}"))
        fmap (\q -> (i18nDefault (qLabel q), qRequired q, qType q)) (findQ s "q1") @?= Just ("Colour?", True, QChoice Single Radio [choice "Red" "Red"])
        (metaTitle (formMeta s), metaDescription (formMeta s)) @?= (Just (plain "T"), Nothing)
        warned "updateItem ignored: no item at index 7" ws
    , testCase "a google forms text item keeps its title and description apart" $ do
        (s, out) <- stable "google-forms" "{\"info\":{\"title\":\"T\"},\"items\":[{\"itemId\":\"n\",\"title\":\"Welcome\",\"description\":\"Read this first.\",\"textItem\":{}}]}"
        [t | ItemNote _ t _ <- formItems s] @?= [plain "Welcome\n\nRead this first."]
        has "\"description\": \"Read this first.\"" out
        has "\"title\": \"Welcome\"" out
    , testCase "gift keeps an option that looks like a weight or a matching pair" $ do
        let q = question "q" "Pick" (QChoice Single Radio [choice "%50%Discount" "%50%Discount", choice "a -> b" "a -> b", choice "plain" "plain"])
        () <$ exact "gift" (one q)
    , testCase "aiken reports text it cannot hold as written" $ do
        let q = (question "q1" "A. Which\n  one?" (QChoice Single Radio [choice "x" "x", choice "y" "y"])) {qAttrs = M.fromList [("aiken:answer", String "A")]}
        (_, ws) <- writeAs "aiken" (one q)
        warned "question text starts like an option" ws
        warned "line breaks and repeated or surrounding spaces written as single spaces" ws
    , testCase "surveydown keeps spaces around labels and reports a reflowed hint" $ do
        let q = (question "q1" " Padded label " (QChoice Single Radio [choice "a" " A ", choice "b" "B"])) {qHint = Just (plain "two\nlines")}
        (out, ws) <- writeAs "surveydown" (one q)
        warned "hint line breaks" ws
        (s, _) <- readAs "surveydown" out
        fmap (i18nDefault . qLabel) (findQ s "q1") @?= Just " Padded label "
        fmap qType (findQ s "q1") @?= Just (QChoice Single Radio [choice "a" " A ", choice "b" "B"])
    ]
