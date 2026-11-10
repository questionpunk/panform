module Main (main) where

import Data.Aeson (Value (..), decode, toJSON, object, (.=), encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.ByteString as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find, isSuffixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Audit
import Panform.Conv
import Panform.Expr
import Panform.Expr.QP (parseQP, printQP)
import Panform.Expr.Simplify (simplify)
import Panform.Format.Qualtrics (stripHtml)
import Panform.Formats
import Panform.Design (designFields, readDesign, writeDesign)
import Panform.Json (key, keyArr)
import qualified ReviewQP
import qualified ReviewA
import qualified ReviewC
import qualified ReviewB
import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  sample <- BL.readFile "examples/customer.surveyjs.json"
  qpFixture <- BL.readFile "examples/customer.qp.json"
  defaultMain $
    testGroup
      "panform"
      [ testGroup "expressions" [exprRoundTrip d | d <- [xlsform, redcap, surveyjs, limesurvey, xpath ("/data/" <>), rlang]]
      , exprExamples
      , qpValueTests
      , ReviewQP.tests
      , simplifyTests
      , marketResearch
      , googleTypeformTests
      , testGroup "writers reach a fixed point" [fixedPoint sample f | f <- formats, Just _ <- [fmtReader f], Just _ <- [fmtWriter f]]
      , standardsFormats
      , questionPunkJsonTests sample qpFixture
      , xformFhirTests sample
      , qualtricsTests
      , textFormats sample
      , limeSurveyTests sample
      , crossFixedPoints
      , sameFormatLossless
      , testGroup "no silent loss" [noSilentLoss f | f <- formats, Just _ <- [fmtWriter f]]
      , msformsTests
      , testProperty "native round trip is lossless" $ forAll genSurvey $ \s ->
          fmap fst (convertVia native s) === Right s
      , saasJsonFormats
      , visualDesignTests
      , ReviewA.tests
      , ReviewC.tests
      , ReviewB.tests
      ]
  where
    native = maybe (error "native format missing") id (lookupFormat "native")
    convertVia f s = runConv $ case (fmtWriter f, fmtReader f) of
      (Just w, Just r) -> w s >>= r
      _ -> failWith "no reader/writer"

-- | Reading a format's own output and writing it again changes nothing:
-- whatever a writer emits, its reader understands in full.
fixedPoint :: BL.ByteString -> FormatSpec -> TestTree
fixedPoint sample f = testCase (fmtName f) $ do
  let sjs = maybe (error "surveyjs format missing") id (lookupFormat "surveyjs")
      once = runConv $ do
        s <- maybe (failWith "") ($ sample) (fmtReader sjs)
        maybe (failWith "") ($ s) (fmtWriter f)
  case once of
    Left e -> assertFailure (show e)
    Right (out1, _) -> case convert f f out1 of
      Left e -> assertFailure (show e)
      Right (out2, _) ->
        let diffAt = length (takeWhile id (BL.zipWith (==) out1 out2))
            ctx o = show (BL.take 160 (BL.drop (fromIntegral (max 0 (diffAt - 40))) o))
         in assertBool ("second write differs at byte " <> show diffAt <> "\n first: " <> ctx out1 <> "\nsecond: " <> ctx out2) (out1 == out2)

-- | GIFT, Aiken, Markdown, HTML, jsPsych and surveydown fixtures.
textFormats :: BL.ByteString -> TestTree
textFormats sample =
  testGroup
    "text formats"
    [ testCase "gift fixture" $ do
        (s, ws) <- readFixture "gift" "examples/cars.gift"
        map groupId [g | ItemGroup g <- formItems s] @?= ["$course$/Cars", "$course$/Trivia"]
        length (questions s) @?= 8
        typeOf s "own" @?= Just (QChoice Single Radio [choice "Yes" "Yes", choice "No" "No"])
        fmap (M.lookup "gift:correct" . qAttrs) (findQ s "own") @?= Just (Just (toJSON ["Yes" :: T.Text]))
        isMulti (typeOf s "brands") @?= True
        fmap (map choiceLabel . choicesOf) (typeOf s "brands") @?= Just (map plain ["Ford", "Toyota", "Honda: Civic"])
        typeOf s "comments" @?= Just (QText LongText)
        typeOf s "handbrake" @?= Just QBoolean
        (findQ s "count" >>= qValidation >>= valMax) @?= Just 20
        isMatrix (typeOf s "parts") @?= True
        typeOf s "q8" @?= Just (QText ShortText)
        fmap (i18nDefault . qLabel) (findQ s "q9") @?= Just "Most family cars carry _____ tyres on the road."
        assertBool "scoring warned once" (length [w | w <- ws, "scoring" `T.isInfixOf` warnWhat w] == 1)
    , testCase "gift writer marks no right answer and escapes" $ do
        out <- writeWith "gift" (Form emptyMeta [ItemQuestion (question "q" "a{b}: c" (QChoice Multiple Radio [choice "x" "x=1", choice "y" "y"]))])
        out @?= "::q:: a\\{b\\}\\: c {\n~%50%x\\=1\n~%50%y\n}\n"
    , testCase "aiken fixture" $ do
        (s, _) <- readFixture "aiken" "examples/quiz.aiken.txt"
        map qId (questions s) @?= ["q1", "q2"]
        fmap (length . choicesOf) (typeOf s "q2") @?= Just 6
        fmap (M.lookup "aiken:answer" . qAttrs) (findQ s "q1") @?= Just (Just (String "D"))
    , testCase "aiken keeps single choice only" $ do
        (_, ws) <- either (assertFailure . show) pure (convertFrom "surveyjs" "aiken" sample)
        assertBool "text question dropped with a warning" (any (\w -> warnWhere w == "why" && "dropped" `T.isInfixOf` warnWhat w) ws)
    , testCase "markdown prints logic in English" $ do
        md <- either (assertFailure . show) (pure . fst) (convertFrom "surveyjs" "markdown" sample)
        assertBool "english logic" ("**Shown if** Q5 is at most 6 and Q5 is answered." `BS.isInfixOf` BL.toStrict md)
    , testCase "html escapes and keeps logic" $ do
        out <- writeWith "html" (Form emptyMeta {metaLanguages = ["fr"]} [ItemQuestion (question "q" "<b>\"x\" & 'y'</b>" (QNumber Integer)) {qRelevant = Just (EBin Eq (ERef "a") (ELit (LStr "o'k")))}])
        let has t = assertBool (T.unpack t) (TE.encodeUtf8 t `BS.isInfixOf` BL.toStrict out)
        has "<html lang=\"fr\">"
        has "&lt;b&gt;&quot;x&quot; &amp; &#39;y&#39;&lt;/b&gt;"
        has "data-relevant=\"{a} = &#39;o\\&#39;k&#39;\""
        has "<input type=\"number\" id=\"q\" name=\"q\" step=\"1\">"
    , testCase "jspsych classic fixture" $ do
        (s, ws) <- readFixture "jspsych" "examples/classic.jspsych.json"
        length [g | ItemGroup g <- formItems s] @?= 5
        [t | ItemNote _ (I18n t _) _ <- allItems (formItems s)] @?= ["Thanks!"]
        map qId (questions s) @?= ["q1", "colour", "q2", "q3", "r1", "r2"]
        fmap (fmap i18nDefault . groupTitle) (listToMaybe [g | ItemGroup g <- formItems s]) @?= Just (Just "About you")
        typeOf s "q3" @?= Just (QText LongText)
        fmap (map choiceValue . choicesOf) (typeOf s "r1") @?= Just ["0", "1", "2"]
        length ws @?= 1
    , testCase "jspsych writes classic trials when lossless, else the survey plugin" $ do
        (s, _) <- readFixture "jspsych" "examples/classic.jspsych.json"
        -- The closing keyboard trial reads as a note, which classic survey
        -- trials cannot hold.
        classic <- writeWith "jspsych" s {formItems = [i | i@(ItemGroup g) <- formItems s, not (null [q | ItemQuestion q <- groupItems g])]}
        assertBool "classic" ("jsPsychSurveyLikert" `BS.isInfixOf` BL.toStrict classic && not ("\"jsPsychSurvey\"" `BS.isInfixOf` BL.toStrict classic))
        (again, _) <- either (assertFailure . show) pure (convertFrom "jspsych" "jspsych" classic)
        again @?= classic
        (plugin, _) <- either (assertFailure . show) pure (convertFrom "surveyjs" "jspsych" sample)
        assertBool "plugin" ("\"jsPsychSurvey\"" `BS.isInfixOf` BL.toStrict plugin)
    , testCase "surveydown fixture" $ do
        (s, ws) <- readFixture "surveydown" "examples/cars.qmd"
        map groupId [g | ItemGroup g <- formItems s] @?= ["welcome", "cars", "end"]
        fmap i18nDefault (metaTitle (formMeta s)) @?= Just "Car survey"
        map qId (questions s) @?= ["vehicle_ownership", "trip_frequency", "weekly_km"]
        typeOf s "trip_frequency"
          @?= Just (QMatrix Single [choice "work_school" "Getting to work or school", choice "food_shop" "The weekly food shop"] [choice "never" "Never", choice "some_days" "Some days", choice "most_days" "Most days"])
        typeOf s "weekly_km" @?= Just (QNumber Decimal)
        map warnWhat ws @?= ["call to sd_close kept raw"]
    , testCase "surveydown logic round trips through the app.R block" $ do
        let viaQmd = convertFrom "surveyjs" "surveydown" sample >>= \(q, _) -> convertFrom "surveydown" "native" q
            rel s i = findQ s i >>= qRelevant
        case (convertFrom "surveyjs" "native" sample, viaQmd) of
          (Right (a, _), Right (b, _)) -> do
            sa <- decodeNative a
            sb <- decodeNative b
            [rel sb i | i <- ["features", "api_lang", "why"]] @?= [rel sa i | i <- ["features", "api_lang", "why"]]
            fmap qRequired (findQ sb "used") @?= Just True
            [groupRelevant g | ItemGroup g <- formItems sb, groupId g == "p2"] @?= [groupRelevant g | ItemGroup g <- formItems sa, groupId g == "p2"]
          (a, b) -> assertFailure (show (fmap snd a, fmap snd b))
    , testCase ".md is markdown, .qp.md stays questionpunk" $
        map (fmap fmtName . formatForPath) ["a.md", "a.qp.md", "a.gift", "a.aiken.txt", "a.jspsych.json", "a.qmd", "a.html"]
          @?= map Just ["markdown", "questionpunk", "gift", "aiken", "jspsych", "surveydown", "html"]
    , testCase "r dialect" $
        (parseExpr surveyjs "{a} = 'yes' and {b} contains 'x' or {c} empty" >>= either (Left . T.unpack) Right . printExpr rlang)
          @?= Right "input$a == \"yes\" & \"x\" %in% input$b | !sd_is_answered(\"c\")"
    ]
  where
    fmt n = maybe (error ("format missing: " <> n)) id (lookupFormat n)
    convertFrom a b = convert (fmt a) (fmt b)
    readFixture n path = do
      bs <- BL.readFile path
      case runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n))) of
        Left e -> assertFailure (show e)
        Right r -> pure r
    writeWith n s = either (assertFailure . show) (pure . fst) (runConv (maybe (failWith "no writer") ($ s) (fmtWriter (fmt n))))
    decodeNative bs = either (assertFailure . show) (pure . fst) (runConv (maybe (failWith "") ($ bs) (fmtReader (fmt "native"))))
    findQ s i = listToMaybe [q | q <- questions s, qId q == i]
    typeOf s i = qType <$> findQ s i
    choicesOf = \case
      QChoice _ _ cs -> cs
      QRanking cs -> cs
      _ -> []
    isMulti = \case
      Just (QChoice Multiple _ _) -> True
      _ -> False
    isMatrix = \case
      Just (QMatrix {}) -> True
      _ -> False

exprExamples :: TestTree
exprExamples =
  testGroup
    "dialect translation"
    [ testCase "surveyjs -> redcap" $ trans surveyjs redcap "{a} = 'yes' and {b} contains 'x'" @?= Right "[a] = 'yes' and [b(x)] = '1'"
    , testCase "xlsform -> surveyjs" $ trans xlsform surveyjs "not(selected(${b}, 'x')) or ${n} >= 3" @?= Right "{b} notcontains 'x' or {n} >= 3"
    , testCase "redcap -> xlsform" $ trans redcap xlsform "[age] > 17 and [c(2)] = '0'" @?= Right "${age} > 17 and not(selected(${c}, '2'))"
    , testCase "not pushed in for redcap" $ printExpr redcap (ENot (EBin Or (EBin Eq (ERef "a") (ELit (LNum 1))) (EBin Lt (ERef "b") (ELit (LNum 2))))) @?= Right "[a] <> 1 and [b] >= 2"
    , testCase "precedence kept" $ trans surveyjs xlsform "({a} = 1 or {b} = 2) and {c} = 3" @?= Right "(${a} = 1 or ${b} = 2) and ${c} = 3"
    , testCase "limesurvey" $ trans surveyjs limesurvey "{a} notempty and {q} = 'A1'" @?= Right "!is_empty(a.NAOK) && q.NAOK == \"A1\""
    ]
  where
    trans a b src = either (Left . T.pack) Right (parseExpr a src) >>= printExpr b

qpValueTests :: TestTree
qpValueTests =
  testGroup
    "questionpunk logic values read back as written"
    [ testCase (show v) $ do
        let e = EBin Eq (ERef "q1") (ELit (LStr v))
        src <- either (assertFailure . T.unpack) pure (printQP e)
        assertBool "a condition stays on one line" (not (T.any (`elem` ("\n\r" :: String)) src))
        parseQP src @?= Right e
    | v <- ["AC\\DC", "C:\\Users\\new", "say \"hi\"", "two\nlines", "tab\there", "back\\\"both", "plain words", "\233\\\252"]
    ]

exprRoundTrip :: Dialect -> TestTree
exprRoundTrip d =testProperty (T.unpack (dName d)) $ forAll genExpr $ \e ->
  case printExpr d e of
    Left _ -> discard
    Right src -> counterexample (T.unpack src) (parseExpr d src === Right e)

genName :: Gen T.Text
genName = T.pack <$> ((:) <$> elements ['a' .. 'z'] <*> listOf (elements (['a' .. 'z'] ++ ['0' .. '9'])))
  `suchThat` (`notElem` ["and", "or", "not", "div", "eq", "ne", "lt", "le", "gt", "ge", "true", "false", "empty", "notempty", "contains", "notcontains", "selected", "is_empty"])

genValue :: Gen T.Text
genValue = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9']))

genExpr :: Gen Expr
genExpr = sized go
  where
    leaf =
      oneof
        [ EBin <$> elements [Eq, Neq, Lt, Le, Gt, Ge] <*> (ERef <$> genName) <*> lit
        , ESelected <$> genName <*> genValue
        , EAnswered <$> genName
        , ENot . EAnswered <$> genName
        ]
    lit = oneof [ELit . LStr <$> genValue, ELit . LNum . fromIntegral <$> (arbitrary :: Gen Int) `suchThat` (>= 0)]
    go n
      | n <= 1 = leaf
      | otherwise = oneof [leaf, EBin <$> elements [And, Or] <*> go (n `div` 2) <*> go (n `div` 2)]

genI18n :: Gen I18n
genI18n = I18n <$> genValue <*> (M.fromList <$> listOf ((,) <$> elements ["fr", "de", "es"] <*> genValue))

genSurvey :: Gen Form
genSurvey = do
  qs <- listOf1 genQuestion
  pure (Form emptyMeta {metaTitle = Just (plain "t")} [ItemGroup (Group "p1" Page Nothing Nothing (map ItemQuestion qs) M.empty)])
  where
    genQuestion = do
      i <- genName
      l <- genI18n
      t <- elements [QText ShortText, QNumber Integer, QBoolean, QChoice Single Radio [choice "a" "A", choice "b" "B"], QScale 1 5 Nothing Nothing]
      r <- arbitrary
      pure (question i "" t) {qLabel = l, qRequired = r}

-- | Triple-S, DDI, ODM, SPSS and QTI read their example files.
standardsFormats :: TestTree
standardsFormats =
  testGroup
    "standards formats"
    [ testCase "triple-s: official 3.0 example" $ do
        s <- readExample "triple-s" "examples/historic-house.sss"
        length (questions s) @?= 12
        metaId (formMeta s) @?= Just "SP5201-1"
        qTypeOf s "Q3" @?= Just (QChoice Multiple Radio [choice "1" "Sherwood Forest", choice "2" "Nottingham Castle", choice "3" "\"Friar Tuck\" Restaurant", choice "4" "\"Maid Marion\" Cafe", choice "5" "Mining museum", choice "9" "Other"])
        fmap (i18nDefault . qLabel) (find' s "Q2") @?= Just "Have you visited here before?"
        (find' s "Q8" >>= qRelevant) @?= Just (EBin Eq (ERef "Q7") (ELit (LNum 1)))
        qTypeOf s "Q7" @?= Just QBoolean
        qTypeOf s "WT" @?= Just QHidden
        (find' s "Q6" >>= qValidation >>= valMax) @?= Just 499
    , testCase "ddi: NADA-style codebook" $ do
        s <- readExample "ddi" "examples/household-finance.ddi.xml"
        length (questions s) @?= 8
        qTypeOf s "year" @?= Just QHidden
        case qTypeOf s "acct1" of
          Just (QChoice Single _ cs) -> map choiceValue cs @?= ["1", "2", "3", "4"]
          other -> assertFailure (show other)
        fmap (T.isPrefixOf "A current account is an account" . i18nDefault . qLabel) (find' s "acct1") @?= Just True
    , testCase "spss: OpenStatSpec dictionary syntax" $ do
        s <- readExample "spss" "examples/openstatspec-dictionary.sps"
        fmap i18nDefault (metaTitle (formMeta s)) @?= Just "OpenStatSpec synthetic dictionary and display fixture"
        case qTypeOf s "contact_modes" of
          Just (QChoice Multiple _ cs) -> map choiceValue cs @?= ["channel_email", "channel_sms", "channel_web"]
          other -> assertFailure (show other)
        case qTypeOf s "satisfaction" of
          Just (QChoice Single _ cs) -> length cs @?= 5
          other -> assertFailure (show other)
        qTypeOf s "income" @?= Just (QNumber Decimal)
        qTypeOf s "interview_date" @?= Just (QDate Date)
        (find' s "ranged_missing" >>= M.lookup "spss:missing" . qAttrs) @?= Just "97 THRU 99"
    , testCase "odm: REDCap project export" $ do
        s <- readExample "odm" "examples/redcap-demographics.odm.xml"
        case qTypeOf s "weekly_schedule" of
          Just (QMatrix Multiple rows cols) -> (length rows, length cols) @?= (4, 5)
          other -> assertFailure (show other)
        (find' s "given_birth" >>= qRelevant) @?= Just (EBin Eq (ERef "sex") (ELit (LStr "0")))
        qTypeOf s "specify_mood" @?= Just (QScale 0 100 (Just (plain "Very sad")) (Just (plain "Very happy")))
        fmap qHint (find' s "telephone_1") @?= Just (Just (plain "Include Area Code"))
        length [g | ItemGroup g <- allItems (formItems s), groupKind g == Section] @?= 5
    , testCase "odm: generic ODM 1.3.2 with conditions (OpenEDC)" $ do
        s <- readExample "odm" "examples/openedc-metadata.odm.xml"
        (find' s "Pregnant" >>= qRelevant) @?= Just (EBin Eq (ERef "Gender") (ELit (LStr "Female")))
        fmap (M.lookup "de" . i18nTranslations . qLabel) (find' s "Age") @?= Just (Just "Wie alt sind Sie?")
        (find' s "Age" >>= qValidation >>= valMin) @?= Just 18
        metaLanguages (formMeta s) @?= ["en", "de"]
    , testCase "qti: sample items with a precondition and a branch rule" $ do
        s <- readExample "qti" "examples/qti-samples.qti.xml"
        length (questions s) @?= 7
        (find' s "choiceMultiple" >>= qRelevant) @?= Just (EBin Eq (ERef "choice") (ELit (LStr "ChoiceA")))
        (find' s "order" >>= qRelevant) @?= Just (EBin Ge (ERef "slider") (ELit (LNum 10)))
        (find' s "match" >>= qRelevant) @?= Nothing
        qTypeOf s "slider" @?= Just (QScale 0 100 Nothing Nothing)
        case qTypeOf s "match" of
          Just (QMatrix Single rows cols) -> (length rows, length cols) @?= (4, 3)
          other -> assertFailure (show other)
    ]
  where
    readExample name path = do
      bs <- BL.readFile path
      case lookupFormat name >>= fmtReader of
        Nothing -> assertFailure ("no reader for " <> name) >> error "unreachable"
        Just r -> either (\e -> assertFailure (show e) >> error "unreachable") (pure . fst) (runConv (r bs))
    find' s i = case [q | q <- questions s, qId q == i] of
      (q : _) -> Just q
      [] -> Nothing
    qTypeOf s i = qType <$> find' s i
-- Forsta (Decipher) XML, Dimensions mrScript and Blaise.
marketResearch :: TestTree
marketResearch =
  testGroup
    "market research formats"
    [ testGroup "dialect round trips" [exprRoundTrip d | d <- [decipher, mrscript, blaise]]
    , testCase "decipher conditions" $ trans decipher surveyjs "q1.r1 and not q2.any or q3.val > 2" @?= Right "{q1} contains 'r1' and {q2} empty or {q3} > 2"
    , testCase "mrscript conditions" $ trans mrscript surveyjs "q1.ContainsAny({a, _2}) And Not IsEmpty(q3)" @?= Right "({q1} contains 'a' or {q1} contains '2') and {q3} notempty"
    , testCase "blaise conditions" $ trans blaise surveyjs "(Train IN Travel) AND Age >= 16 AND Name = RESPONSE" @?= Right "{Travel} contains 'Train' and {Age} >= 16 and {Name} notempty"
    , fixture "decipher" "examples/brand-tracker.decipher.xml" $ \s -> do
        map qId (questions s) @?= ["S1", "Q1", "Q2", "Q3", "Q4", "Q5", "hCups", "Q6"]
        [i | ItemNote i _ _ <- allItems (formItems s)] @?= ["intro"]
        case qType (q s "Q1") of
          QChoice Multiple _ cs -> [(choiceValue c, choiceOther c, choiceExclusive c) | c <- cs, choiceOther c || choiceExclusive c] @?= [("r97", True, False), ("r98", False, True)]
          t -> assertFailure (show t)
        case qType (q s "Q3") of
          QMatrix Single rs cs -> (length rs, map choiceValue cs) @?= (2, ["1", "2", "3"])
          t -> assertFailure (show t)
        qType (q s "Q2") @?= QChoice Single Dropdown [choice "ch1" "Blue Bottle", choice "ch2" "Lavazza", choice "ch3" "Starbucks"]
        (qValidation (q s "Q4") >>= valMax) @?= Just 50
        logic (qRelevant (q s "Q5")) @?= Just "{Q1} contains 'r3' and {Q4} > 10"
        logic (groupRelevant (g s "coffee")) @?= Just "{S1} = 'r1'"
        (groupKind (g s "coffee"), groupTitle (g s "coffee")) @?= (Page, Just (plain "Coffee habits"))
        logic (case qType (q s "hCups") of QCalculate e -> Just e; _ -> Nothing) @?= Just "{Q4} * 4"
        (qRequired (q s "Q5"), qRequired (q s "S1"), qType (q s "Q6")) @?= (False, True, QText Email)
    , fixture "mrscript" "examples/museum.mrs" $ \s -> do
        map qId (questions s) @?= ["Visited", "Galleries", "RatingLoop", "Age", "Spend", "Member", "VisitDate", "Comments", "Region", "AgeGroup"]
        i18nTranslations (qLabel (q s "Visited")) @?= M.fromList [("es-ES", "\191Es su primera visita?")]
        metaLanguages (formMeta s) @?= ["en-US", "es-ES"]
        logic (qRelevant (q s "RatingLoop")) @?= Just "{Visited} = 'No' and ({Galleries} contains 'Fossils' or {Galleries} contains 'Dinosaurs')"
        logic (qRelevant (q s "Comments")) @?= Just "{Age} > 64 or {Comments} notempty"
        (qRequired (q s "Comments"), qRequired (q s "Age")) @?= (False, True)
        case qType (q s "Galleries") of
          QChoice Multiple _ cs -> [(choiceValue c, choiceOther c, choiceExclusive c) | c <- cs, choiceOther c || choiceExclusive c] @?= [("other", True, False), ("None", False, True)]
          t -> assertFailure (show t)
        case qType (q s "RatingLoop") of
          QMatrix Single rs cs -> (map choiceValue rs, map choiceValue cs) @?= (["Fossils", "Dinosaurs"], ["1", "2", "3"])
          t -> assertFailure (show t)
        (qType (q s "AgeGroup"), qType (q s "Member"), qValidation (q s "Age") >>= valMin) @?= (QHidden, QBoolean, Just 16)
        case qType (q s "Region") of
          QChoice Single Dropdown cs -> length cs @?= 4
          t -> assertFailure (show t)
        map itemId (groupItems (g s "About")) @?= ["Age", "Spend", "Member", "VisitDate"]
    , fixture "blaise" "examples/commute.bla" $ \s -> do
        map qId (questions s) @?= ["Nickname", "Site", "Tenure", "Started", "OnSite", "Modes", "Minutes", "Permits", "Remarks", "TenureBand"]
        metaLanguages (formMeta s) @?= ["eng", "ned"]
        i18nTranslations (qLabel (q s "Nickname")) @?= M.fromList [("ned", "Hoe mogen we u noemen?")]
        case qType (q s "Modes") of
          QChoice Multiple _ cs -> map choiceValue cs @?= ["0", "1", "2", "4", "5", "6", "9"]
          t -> assertFailure (show t)
        logic (qRelevant (q s "Minutes")) @?= Just "{Tenure} >= 1 and {OnSite} = 'Yes' and ({Modes} contains '2' or {Modes} contains '4')"
        logic (qRelevant (q s "Permits")) @?= Just "{Site} = 'South' and {Tenure} >= 2"
        (qRequired (q s "Minutes"), qRequired (q s "Nickname"), qType (q s "Remarks"), qType (q s "TenureBand")) @?= (False, True, QText LongText, QHidden)
        (qType (q s "Minutes"), qType (q s "Started")) @?= (QNumber Decimal, QDate Date)
        [i | ItemNote i _ _ <- formItems s] @?= ["Intro"]
    ]
  where
    trans a b src = either (Left . T.pack) Right (parseExpr a src) >>= printExpr b
    logic = fmap (either id id . printExpr surveyjs)
    q s i = maybe (error ("no question " <> T.unpack i)) id (lookup i [(qId x, x) | x <- questions s])
    g s i = maybe (error ("no group " <> T.unpack i)) id (lookup i [(groupId x, x) | ItemGroup x <- allItems (formItems s)])
    fixture f path check = testCase (f <> " fixture") $ do
      bs <- BL.readFile path
      case lookupFormat f >>= fmtReader of
        Nothing -> assertFailure "no reader"
        Just r -> either (assertFailure . show) (check . fst) (runConv (r bs))

-- | SurveyMonkey, Jotform, Tally and Formbricks fixtures read into the
-- IR with their types, codes and logic intact.
saasJsonFormats :: TestTree
saasJsonFormats =
  testGroup
    "saas json formats"
    [ testCase "formbricks" $ do
        (s, ws) <- readFixture "formbricks" "examples/onboarding.formbricks.json"
        length (questions s) @?= 9
        metaLanguages (formMeta s) @?= ["en", "de"]
        typeOf s "plan" @?= Just QHidden
        typeOf s "nps" @?= Just (QScale 0 10 (Just (I18n "Not likely" (M.fromList [("de", "Unwahrscheinlich")]))) (Just (I18n "Very likely" (M.fromList [("de", "Sehr wahrscheinlich")]))))
        typeOf s "contact_ok" @?= Just QBoolean
        fmap (map choiceOther) (choicesOf s "role") @?= Just [False, False, True]
        pageRel s "blkstack" @?= Just (Just (EBin Neq (ERef "role") (ELit (LStr "pm"))))
        pageRel s "blkwhy" @?= Just (Just (EBin And (EBin Lt (ERef "nps") (ELit (LNum 9))) (EAnswered "ease")))
        assertBool "calculate action warned" (any ((== "calculate action dropped") . warnWhat) ws)
    , testCase "surveymonkey" $ do
        (s, ws) <- readFixture "surveymonkey" "examples/pets.surveymonkey.json"
        length (questions s) @?= 7
        typeOf s "q81100006" @?= Just (QScale 0 10 (Just (plain "Not at all likely - 0")) (Just (plain "Extremely likely - 10")))
        typeOf s "q81100003" @?= Just (QNumber Integer)
        fmap qValidation (lookupQ s "q81100003") @?= Just (Just (Validation (Just 0) (Just 24) Nothing (Just (plain "Please enter a whole number between 0 and 24."))))
        case typeOf s "q81100005" of
          Just (QMatrix Single rows cols) -> (length rows, map choiceValue cols) @?= (2, ["1", "2", "3"])
          other -> assertFailure (show other)
        fmap (map choiceValue) (choicesOf s "q81100001") @?= Just ["501", "502", "other"]
        assertBool "deleted question warned" (any ((== "deleted question skipped") . warnWhat) ws)
    , testCase "jotform" $ do
        (s, ws) <- readFixture "jotform" "examples/event.jotform.json"
        length (questions s) @?= 9
        fmap (map choiceValue) (choicesOf s "track") @?= Just ["1", "2", "3"]
        fmap qRelevant (lookupQ s "workshops") @?= Just (Just (EBin Or (EBin Eq (ERef "track") (ELit (LStr "2"))) (EBin Eq (ERef "track") (ELit (LStr "3")))))
        fmap qRelevant (lookupQ s "whatWentWrong") @?= Just (Just (EBin Lt (ERef "overall") (ELit (LNum 3))))
        typeOf s "overall" @?= Just (QScale 1 5 (Just (plain "Poor")) (Just (plain "Great")))
        [(groupId g, groupTitle g) | ItemGroup g <- formItems s] @?= [("page1", Just (plain "Event registration")), ("feedback", Just (plain "Your feedback"))]
        assertBool "page skip warned" (any ((== "page condition dropped") . warnWhat) ws)
    , testCase "tally" $ do
        (s, _) <- readFixture "tally" "examples/signup.tally.json"
        length (questions s) @?= 7
        typeOf s "happiness" @?= Just (QScale 1 7 (Just (plain "Unhappy")) (Just (plain "Delighted")))
        fmap qRelevant (lookupQ s "company") @?= Just (Just (EBin Neq (ERef "team_size") (ELit (LStr "2-10"))))
        pageRel s "tools" @?= Just (Just (ENot (EBin Eq (ERef "team_size") (ELit (LStr "Just me")))))
        fmap (map choiceOther) (choicesOf s "tools") @?= Just [False, False, True]
        fmap qValidation (lookupQ s "company") @?= Just (Just (Validation Nothing (Just 80) Nothing Nothing))
    ]
  where
    readFixture name path = do
      bs <- BL.readFile path
      case lookupFormat name >>= fmtReader of
        Nothing -> assertFailure ("no reader for " <> name)
        Just r -> either (assertFailure . show) pure (runConv (r bs))
    lookupQ s i = case [q | q <- questions s, qId q == i] of
      (q : _) -> Just q
      [] -> Nothing
    typeOf s i = qType <$> lookupQ s i
    choicesOf s i = case typeOf s i of
      Just (QChoice _ _ cs) -> Just cs
      _ -> Nothing
    pageRel s i = case [g | ItemGroup g <- formItems s, groupId g == i] of
      (g : _) -> Just (groupRelevant g)
      [] -> Nothing



-- | questionpunk-json: the QuestionPunk app's chat-editor payload.
questionPunkJsonTests :: BL.ByteString -> BL.ByteString -> TestTree
questionPunkJsonTests sample fixture =
  testGroup
    "questionpunk-json"
    [ testCase "reads the app fixture" $ do
        s <- either (assertFailure . show) (pure . fst) (runConv (maybe (failWith "no reader") ($ fixture) (fmtReader qp)))
        let byId i = [q | q <- questions s, qId q == i]
            groups = [g | ItemGroup g <- formItems s]
        length (allItems (formItems s)) - length groups @?= 14
        map groupTitle groups @?= map (Just . plain) ["Screener", "Your tool", "Deep dive", "About your team"]
        map qType (byId "drivers") @?= [QOther "max_diff"]
        map qType (byId "pain_interview") @?= [QOther "ai_interview"]
        map (M.lookup "questionpunk:questionMode" . qAttrs) (byId "pain_interview") @?= [Just (String "ai_decides")]
        map qType (byId "mobile_rating") @?= [QScale 1 5 (Just (plain "Poor")) (Just (plain "Excellent"))]
        map qType (byId "team_size") @?= [QNumber Integer]
        -- Branching "after Q2, if No go to Q3, else Q4" becomes a condition on Q3.
        map qRelevant (byId "q3") @?= [Just (EBin Eq (ERef "uses_tool") (ELit (LStr "No")))]
        map qRelevant (byId "improvements") @?= [Just (EBin Or (ESelected "features" "Boards") (ESelected "features" "Timelines"))]
        map qRelevant (byId "pain_interview") @?= [Just (EBin Lt (ERef "satisfaction") (ELit (LNum 3)))]
        M.keys (metaAttrs (formMeta s)) @?= ["questionpunk:thank_screen_text", "questionpunk:welcome_screen_text"]
    , testCase "end-of-survey message goes back out as branching" $ do
        out <- either (assertFailure . show) (pure . fst) (convert qp qp fixture)
        field "survey_logic" out
          @?= decode "[{\"source_question\": 2, \"fallback_question\": 4, \"groups\": [{\"condition\": \"or\", \"destination_question\": 3, \"rules\": [{\"type\": \"is_selected_option\", \"option_index\": 1}]}]}]"
    , testCase "QuestionPunk-only types survive QuestionPunk Markdown" $
        case (convert qp qp fixture, convert qp md fixture >>= convert md qp . fst) of
          (Right (direct, _), Right (through, _)) -> through @?= direct
          (a, b) -> assertFailure (show (fmap snd a, fmap snd b))
    , testCase "Markdown takes bare item-detail keys" $ do
        let src = "1. Tell us about your week {#week type=ai_interview questionMode=ai_decides}\n"
        (out, _) <- either (assertFailure . show) pure (convert md qp src)
        (field "survey_items" out >>= nth 0 >>= \case
            Object o -> KM.lookup "itemDetails" o
            _ -> Nothing)
          @?= decode "{\"questionMode\": \"ai_decides\"}"
        (back, _) <- either (assertFailure . show) pure (convert qp md out)
        assertBool (show back) ("questionMode=ai_decides}" `BL.isSuffixOf` BL.takeWhile (/= 10) back)
    , testCase "surveyjs logic becomes app rules" $ do
        (out, ws) <- either (assertFailure . show) pure (convert sjs qp sample)
        -- {features} contains 'api' -> option 2 of question 2; the why
        -- condition keeps {used} = 'yes' and {nps} <= 6 as "less than 7".
        (field "survey_display_logic" out >>= nth 1)
          @?= decode "{\"target_question\": 3, \"container_condition\": \"or\", \"groups\": [{\"source_question\": 2, \"condition\": \"or\", \"rules\": [{\"type\": \"is_selected_option\", \"option_index\": 2}]}]}"
        (field "survey_display_logic" out >>= nth 5)
          @?= decode "{\"target_question\": 7, \"container_condition\": \"and\", \"groups\": [{\"source_question\": 1, \"condition\": \"or\", \"rules\": [{\"type\": \"is_selected_option\", \"option_index\": 0}]}, {\"source_question\": 5, \"condition\": \"or\", \"rules\": [{\"type\": \"is_number_less\", \"value\": 7}]}]}"
        assertBool "translations reported" (any ((== "survey") . warnWhere) ws)
        assertBool "dropped 'is answered' reported" (any ((== "why") . warnWhere) ws)
    ]
  where
    fmt n = maybe (error (n <> " format missing")) id (lookupFormat n)
    qp = fmt "questionpunk-json"
    md = fmt "questionpunk"
    sjs = fmt "surveyjs"
    field k out = case decode out of
      Just (Object o) -> KM.lookup k o
      _ -> Nothing
    nth i = \case
      Array a -> case drop i (toList a) of
        (x : _) -> Just x
        [] -> Nothing
      _ -> Nothing
------------------------------------------------------------------------------
-- Google Forms and Typeform

googleTypeformTests :: TestTree
googleTypeformTests =
  testGroup
    "google forms and typeform"
    [ testCase "google forms fixture" $ do
        s <- readFixture "google-forms" "examples/cars.gform.json"
        length (questions s) @?= 9
        fmap qType (qOf "1e2f3a4b" s) @?= Just (QChoice Single Dropdown [choice "City" "City", choice "Suburb" "Suburb", choice "Countryside" "Countryside"])
        fmap qType (qOf "77aa88bb" s) @?= Just (QScale 1 5 Nothing Nothing)
        case fmap qType (qOf "8e9f0a1b" s) of
          Just (QMatrix Single rows _) -> map choiceValue rows @?= ["11aa22bb", "33cc44dd"]
          t -> assertFailure (show t)
        groupRel "4b5c6d7e" s @?= Just (Just (EBin Neq (ERef "5d6e7f80") (ELit (LStr "No"))))
        groupRel "6a7b8c9d" s @?= Just Nothing
    , testCase "google forms batch replays to the same form" $ do
        s <- readFixture "google-forms" "examples/cars.gform.json"
        s' <- via "google-forms-batch" s
        map qId (questions s') @?= map qId (questions s)
        groupRel "4b5c6d7e" s' @?= groupRel "4b5c6d7e" s
    , testCase "google forms writes page logic as a section jump" $ do
        s <- readFixture "surveyjs" "examples/customer.surveyjs.json"
        s' <- via "google-forms" s
        groupRel "p2" s' @?= Just (Just (EBin Neq (ERef "used") (ELit (LStr "No"))))
    , testCase "typeform fixture" $ do
        s <- readFixture "typeform" "examples/coffee.typeform.json"
        length (questions s) @?= 13
        [qId q | q <- questions s, qType q == QHidden] @?= ["source", "customer_id"]
        fmap qRelevant (qOf "why_not" s) @?= Just (Just (EBin Eq (ERef "drinks_coffee") (ELit (LBool False))))
        fmap qRelevant (qOf "cups" s)
          @?= Just (Just (EBin And (EBin Neq (ERef "drinks_coffee") (ELit (LBool False))) (EBin Neq (ERef "how_often") (ELit (LStr "rarely")))))
        groupRel "cafe" s @?= fmap qRelevant (qOf "cups" s)
        fmap qType (qOf "sweetness" s) @?= Just (QScale 1 5 (Just (plain "Bitter")) (Just (plain "Sweet")))
        assertBool "variable action kept" (M.member "typeform:logic" (metaAttrs (formMeta s)))
    , -- Real forms.get output, from github.com/Vort3xed/FormGen (MIT),
      -- retrieval/retrieved_game.json. Its one jump goes backward.
      testCase "google forms real export" $ do
        (s, ws) <- readFixtureW "google-forms" "examples/escape-room.gform.json"
        length (questions s) @?= 3
        length [g | ItemGroup g <- formItems s, groupKind g == Page] @?= 5
        assertBool "backward jump reported" (any (T.isInfixOf "backward jump" . warnWhat) ws)
        [groupRelevant g | ItemGroup g <- formItems s] @?= replicate 5 Nothing
    , testCase "google forms real export reaches a fixed point" $ selfFixed "google-forms" "examples/escape-room.gform.json"
    , -- Real Typeform GETs, from github.com/vlab-research/fly (MIT),
      -- facebot/testrunner/forms/jISElk.json and nFgfNE.json.
      testCase "typeform real export: branch on a choice" $ do
        s <- readFixture "typeform" "examples/fly-branch.typeform.json"
        let gender = "b424c182-3ebb-4fd2-a1d0-0bec39509630"
            male = EBin Eq (ERef gender) (ELit (LStr "a20110f0-a1b6-42ac-ad7d-c576fa10c59c"))
        fmap qRelevant (qOf "49f1ba88-df11-4711-9d9b-f0855fd648cc" s) @?= Just (Just male)
        fmap qRelevant (qOf "8023a406-3e63-49c4-bd79-44f20abb4996" s) @?= Just (Just (EBin Neq (ERef gender) (ELit (LStr "a20110f0-a1b6-42ac-ad7d-c576fa10c59c"))))
        fmap qType (qOf "ba516856-03b7-4e28-82c2-affc9080fd85" s) @?= Just (QScale 0 10 Nothing Nothing)
    , testCase "typeform real export: branch on a hidden field" $ do
        (s, ws) <- readFixtureW "typeform" "examples/fly-seed.typeform.json"
        [qId q | q <- questions s, qType q == QHidden] @?= ["name", "seed_2"]
        assertBool "branches are conditional" (all (\i -> maybe False (isJustE . qRelevant) (qOf i s)) ["8b67d18c-cda5-4936-83ea-bda055cf20dc", "2641788b-af67-4f0f-ba1c-45fee515443c"])
        assertBool "ending choice reported" (any (T.isInfixOf "ending screen" . warnWhat) ws)
        assertBool "ending fields kept" (M.member "typeform:ending_fields" (metaAttrs (formMeta s)))
    , testCase "typeform real exports reach a fixed point" $ do
        selfFixed "typeform" "examples/fly-branch.typeform.json"
        selfFixed "typeform" "examples/fly-seed.typeform.json"
    , testCase "typeform jumps survive a round trip" $ do
        s <- readFixture "typeform" "examples/coffee.typeform.json"
        s' <- via "typeform" s
        map qRelevant (questions s') @?= map qRelevant (questions s)
    , testCase "typeform writes show-if as jumps" $ do
        s <- readFixture "surveyjs" "examples/customer.surveyjs.json"
        s' <- via "typeform" s
        fmap qRelevant (qOf "api_lang" s') @?= Just (Just (ESelected "features" "api"))
        groupRel "p2" s' @?= Just (Just (EBin Eq (ERef "used") (ELit (LStr "yes"))))
    ]
  where
    fmt n = maybe (error ("format missing: " <> n)) id (lookupFormat n)
    readFixture n path = do
      bs <- BL.readFile path
      case runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n))) of
        Right (s, _) -> pure s
        Left e -> assertFailure (show e) >> error "unreachable"
    via n s = case runConv (maybe (failWith "") ($ s) (fmtWriter (fmt n)) >>= maybe (failWith "") id . (\bs -> ($ bs) <$> fmtReader (fmt n))) of
      Right (s', _) -> pure s'
      Left e -> assertFailure (show e) >> error "unreachable"
    readFixtureW n path = do
      bs <- BL.readFile path
      either (\e -> assertFailure (show e) >> error "unreachable") pure (runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n))))
    selfFixed n path = do
      bs <- BL.readFile path
      case convert (fmt n) (fmt n) bs of
        Left e -> assertFailure (show e)
        Right (out1, _) -> fmap fst (convert (fmt n) (fmt n) out1) @?= Right out1
    isJustE = maybe False (const True)
    qOf i s = find ((== i) . qId) (questions s)
    groupRel i s = groupRelevant <$> find ((== i) . groupId) [g | ItemGroup g <- allItems (formItems s)]
-- ODK XForms and FHIR Questionnaire

xformFhirTests :: BL.ByteString -> TestTree
xformFhirTests sample =
  testGroup
    "xforms and fhir"
    [ testCase "xpath -> xlsform" $ trans (xpath ("/data/" <>)) xlsform "selected( /data/q1 , 'yes') and /data/g/age >= 18 and ../n != ''" @?= Right "selected(${q1}, 'yes') and ${age} >= 18 and ${n} != ''"
    , testCase "pyxform widgets fixture" $ do
        s <- readFixture "xform" "examples/pyxform-widgets.xform.xml"
        let qs = byId s
        length (questions s) @?= 33
        fmap qType (M.lookup "my_int" qs) @?= Just (QNumber Integer)
        fmap qType (M.lookup "my_trigger" qs) @?= Just QBoolean
        fmap qType (M.lookup "my_image" qs) @?= Just QFile
        fmap qDefault (M.lookup "my_decimal" qs) @?= Just (Just "18.31")
        case fmap qType (M.lookup "my_select" qs) of
          Just (QChoice Multiple _ cs) -> map choiceValue cs @?= ["a", "b", "c", "d"]
          t -> assertFailure (show t)
        fmap (fmap valMessage . qValidation) (M.lookup "my_int" qs) @?= Just (Just (Just (plain "number must be less than 10")))
        -- A table-list with a header row stays a group: the header is a
        -- question of its own.
        [(groupKind g, length (groupItems g)) | ItemGroup g <- allItems (formItems s), groupId g == "table_list_test"] @?= [(Page, 3)]
    , testCase "pyxform spec fixture: groups, repeat, itext, logic" $ do
        s <- readFixture "xform" "examples/pyxform-spec-test.xform.xml"
        let gs = M.fromList [(groupId g, g) | ItemGroup g <- allItems (formItems s)]
        fmap groupRelevant (M.lookup "everything" gs) @?= Just (Just (ENot (ESelected "skip_to_end" "yes")))
        fmap groupKind (M.lookup "repeat_test" gs) @?= Just Repeat
        fmap (M.keys . i18nTranslations . qLabel) (M.lookup "address" (byId s)) @?= Just ["chinese", "english"]
        fmap qType (M.lookup "start" (byId s)) @?= Just QHidden
    , testCase "pyxform spec fixture reaches a fixed point" $ selfFixedPoint "xform" "examples/pyxform-spec-test.xform.xml"
    , testCase "xform writer resolves paths in logic" $ do
        out <- either (assertFailure . show) (pure . fst) (convert (fmt "surveyjs") (fmt "xform") sample)
        assertBool "selected() on the nested path" ("relevant=\"selected(/data/p1/features, 'api')\"" `contains` out)
    , testCase "SDC PHQ-9 fixture" $ do
        s <- readFixture "fhir" "examples/sdc-phq9.fhir.json"
        let qs = questions s
        length qs @?= 10
        length [() | q <- qs, QChoice Single _ cs <- [qType q], length cs == 4] @?= 9
        fmap qType (M.lookup "H1/TS" (byId s)) @?= Just (QCalculate (ERaw "fhirpath" "%resource.item.where(linkId!='H1/TS').answer.value.weight().sum()"))
    , testCase "SDC PHQ-9 fixture reaches a fixed point" $ selfFixedPoint "fhir" "examples/sdc-phq9.fhir.json"
    , testCase "zika fixture enableWhen" $ do
        s <- readFixture "fhir" "examples/zika-exposure.fhir.json"
        length (questions s) @?= 6
        fmap qRelevant (M.lookup "2" (byId s)) @?= Just (Just (EBin Eq (ERef "1") (ELit (LBool False))))
    , testCase "fhir enableWhen round trip" $ do
        s <- either (assertFailure . show) (pure . fst) (runConv (reader "surveyjs" sample >>= writer "fhir" >>= reader "fhir"))
        let qs = byId s
        fmap qRelevant (M.lookup "why" qs) @?= Just (Just (EBin And (EBin Le (ERef "nps") (ELit (LNum 6))) (EAnswered "nps")))
        fmap qRelevant (M.lookup "api_lang" qs) @?= Just (Just (ESelected "features" "api"))
        fmap qType (M.lookup "nps" qs) @?= Just (QScale 0 10 (Just (plain "Not likely")) (Just (plain "Very likely")))
        case fmap qType (M.lookup "sat" qs) of
          Just (QMatrix Single rows cols) -> (map choiceValue rows, length cols) @?= (["speed", "price"], 3)
          t -> assertFailure (show t)
    ]
  where
    fmt n = maybe (error ("format missing: " <> n)) id (lookupFormat n)
    trans a b src = either (Left . T.pack) Right (parseExpr a src) >>= printExpr b
    byId s = M.fromList [(qId q, q) | q <- questions s]
    readFixture n path = do
      bs <- BL.readFile path
      either (\e -> assertFailure (show e) >> error "unreachable") (pure . fst) (runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n))))
    selfFixedPoint n path = do
      bs <- BL.readFile path
      case convert (fmt n) (fmt n) bs of
        Left e -> assertFailure (show e)
        Right (out1, _) -> fmap fst (convert (fmt n) (fmt n) out1) @?= Right out1
    contains needle hay = BL.toStrict needle `B.isInfixOf` BL.toStrict hay
    reader n = maybe (const (failWith "no reader")) id (fmtReader (fmt n))
    writer n = maybe (const (failWith "no writer")) id (fmtWriter (fmt n))
-- Qualtrics QSF and TXT

-- | The QSF fixture is Delphi's COVID-19 Trends and Impact Survey, wave 4
-- (github.com/cmu-delphi/delphi-epidata, MIT licence): 8 languages, display
-- logic, a screen-out branch, a form and a text-entry matrix.
qualtricsTests :: TestTree
qualtricsTests =
  testGroup
    "qualtrics"
    [ testCase "qsf: real export reads" $ do
        s <- readWith "qsf" "examples/covid-symptom-survey-wave4.qsf"
        let qs = questions s
            q i = [x | x <- qs, qId x == i]
        length qs @?= 61
        length [() | ItemGroup g <- allItems (formItems s), groupKind g == Page] @?= 13
        length [() | x <- qs, qType x == QHidden] @?= 6
        metaLanguages (formMeta s) @?= ["en", "en-gb", "es", "es-es", "fr", "pt-br", "vi", "zh-s"]
        case q "A1" of
          [x] | QMatrix Single rows cols <- qType x -> do
            length rows @?= 5
            map (i18nDefault . choiceLabel) cols @?= ["Yes", "No"]
            M.member "fr" (i18nTranslations (qLabel x)) @? "A1 has a French label"
          _ -> assertFailure "A1 is not a single-answer matrix"
        map qRelevant (q "B2c") @?= [Just (EBin And (ENot (ESelected "B2" "15")) (EAnswered "B2"))]
        map qRelevant (q "B10a") @?= [Just (EBin Eq (ERef "B10") (ELit (LStr "1")))]
        map qType (q "A4") @?= [QNumber Decimal]
        map qValidation (q "A4") @?= [Just (Validation (Just 0) Nothing Nothing Nothing)]
        map (i18nDefault . qLabel) (q "A2") @?= ["How many people in your household (including yourself) are sick (fever, along with at least one other symptom from the above list)?"]
    , testCase "qsf: write then read keeps the survey" $ do
        s <- readWith "qsf" "examples/covid-symptom-survey-wave4.qsf"
        case runConv (maybe (failWith "") ($ s) (fmtWriter (fmt "qsf")) >>= maybe (const (failWith "")) id (fmtReader (fmt "qsf"))) of
          Left e -> assertFailure (show e)
          Right (s', _) -> noAttrs s' @?= noAttrs s
    , testCase "qsf: html is stripped" $ do
        stripHtml "<p>Hello&nbsp;<b>world</b></p><p>Next &amp; last</p>" @?= ("Hello world\nNext & last", True)
        stripHtml "a &lt; b<br>c" @?= ("a < b\nc", False)
    , testCase "qualtrics-txt: advanced format reads" $ do
        s <- readWith "qualtrics-txt" "examples/qualtrics-advanced.qualtrics.txt"
        let qs = questions s
            q i = [x | x <- qs, qId x == i]
        [(qId x, qDefault x) | x <- qs, qType x == QHidden] @?= [("Branch", Nothing), ("Channel", Nothing), ("Returning", Just "1")]
        case map qType (q "q1") of
          [QChoice Single Radio cs] -> do
            map choiceValue cs @?= ["1", "2", "99"]
            map (i18nDefault . choiceLabel) cs @?= ["I walked past", "Someone I know told me", "Another way,\nsuch as a map or a review"]
          _ -> assertFailure "q1 is not a single choice"
        case [qType x | x <- qs, QMatrix {} <- [qType x]] of
          [QMatrix Single rows cols] -> do
            length rows @?= 3
            map choiceValue cols @?= ["1", "2", "5"]
          _ -> assertFailure "no matrix"
        map qType (q "name") @?= [QText ShortText]
        length [() | QRanking _ <- map qType qs] @?= 1
        length [() | ItemNote {} <- allItems (formItems s)] @?= 1
        length [() | ItemGroup g <- allItems (formItems s), groupKind g == Page] @?= 3
    ]
  where
    fmt n = maybe (error ("format missing: " <> n)) id (lookupFormat n)
    readWith n path = do
      bs <- BL.readFile path
      case runConv (maybe (failWith "") ($ bs) (fmtReader (fmt n))) of
        Left e -> assertFailure (show e) >> fail "unreachable"
        Right (s, _) -> pure s
    noAttrs (Form m is) = Form m (map item is)
    item = \case
      ItemGroup g -> ItemGroup g {groupItems = map item (groupItems g), groupAttrs = M.empty}
      ItemQuestion q -> ItemQuestion q {qAttrs = M.empty}
      i -> i

-- LimeSurvey

-- | Fixtures: ls6-citric.lss is citric's example export (DBVersion 641);
-- gpl/limesurvey/ls2-group-relevance.lss is LimeSurvey 2.05's group
-- relevance demo (DBVersion 177, texts in the structural tables);
-- gpl/limesurvey/ls4-answer-options-multilingual.lss is a LimeSurvey test
-- survey (DBVersion 477, *_l10ns tables, en/nl/fr); both are GPL-2.0-or-later
-- (examples/gpl/README.md). colors.limesurvey.txt is a hand-written file in
-- the layout of a LimeSurvey TSV export.
limeSurveyTests :: BL.ByteString -> TestTree
limeSurveyTests sample =
  testGroup
    "limesurvey"
    [ testCase "6.x .lss" $ do
        (s, _) <- readAs "limesurvey" "examples/limesurvey/ls6-citric.lss"
        map qType (questions s) @?= [QText LongText, QScale 1 5 Nothing Nothing, QFile, QBoolean]
        map qRequired (questions s) @?= [False, True, False, True]
        fmap i18nDefault (qHint =<< firstQ s) @?= Just "This is a question help text."
        length [g | ItemGroup g <- formItems s] @?= 2
    , testCase "pre-4.x .lss with group relevance" $ do
        (s, _) <- readAs "limesurvey" "examples/gpl/limesurvey/ls2-group-relevance.lss"
        let groups = [g | ItemGroup g <- formItems s]
        map (fmap i18nDefault . groupTitle) groups @?= map Just ["Initial Questions", "Person 1", "Person 2", "Person 3", "Person 4", "Summary", "Finished"]
        map groupRelevant (take 2 (drop 1 groups))
          @?= [ Just (EBin Gt (ERef "cohabs") (ELit (LNum 0)))
              , Just (EBin And (EBin Gt (ERef "cohabs") (ELit (LNum 1))) (EAnswered "p1rel"))
              ]
        [qId q | q <- questions s, qType q == QNumber Decimal] @?= ["age", "p1age", "p2age", "p3age", "p4age"]
        [qType q | q <- questions s, qId q == "cohabs"] @?= [QNumber Integer]
    , testCase "4.x .lss with translated answers" $ do
        (s, _) <- readAs "limesurvey" "examples/gpl/limesurvey/ls4-answer-options-multilingual.lss"
        metaLanguages (formMeta s) @?= ["en", "nl", "fr"]
        case map qType (questions s) of
          [QChoice Single Radio cs, QMatrix Single [_] cols] -> do
            length cs @?= 7
            M.lookup "nl" . i18nTranslations . choiceLabel <$> firstOf cs @?= Just (Just "Erg onbelangrijk")
            cols @?= cs
          ts -> assertFailure (show ts)
    , testCase "tab-separated export" $ do
        (s, ws) <- readAs "limesurvey-tsv" "examples/limesurvey/colors.limesurvey.txt"
        case questions s of
          (q0 : q1 : _) -> do
            case qType q0 of
              QChoice Multiple Radio cs -> length cs @?= 11
              t -> assertFailure (show t)
            qRelevant q1 @?= Just (EBin And (ESelected "Q00" "SQ009") (ESelected "Q00" "SQ011"))
          qs -> assertFailure (show (length qs))
        assertBool "quota rows reported" (any (T.isInfixOf "QTA" . warnWhat) ws)
    , testCase "tsv conditions and translations" $ do
        (s, _) <- either (assertFailure . show) pure (runConv (readLimeSurveyTsv' conditionsTsv))
        case questions s of
          [q1, q2, q3] -> do
            M.lookup "de" (i18nTranslations (qLabel q1)) @?= Just "Hast du ein Auto?"
            qRelevant q2 @?= Just (EBin Eq (ERef "car") (ELit (LStr "Y")))
            qRelevant q3 @?= Just (EBin Or (EBin And (EBin Eq (ERef "car") (ELit (LStr "Y"))) (EBin Gt (ERef "cars") (ELit (LNum 2)))) (EBin Eq (ERef "car") (ELit (LStr "N"))))
            M.lookup "de" (i18nTranslations (qLabel q3)) @?= Just "Warum?"
          qs -> assertFailure (show (map qId qs))
    , testCase "writers keep translations and rename codes" $ do
        let lss = maybe (error "limesurvey format missing") id (lookupFormat "limesurvey")
            sjs = maybe (error "surveyjs format missing") id (lookupFormat "surveyjs")
        case runConv (maybe (failWith "") ($ sample) (fmtReader sjs) >>= maybe (const (failWith "")) id (fmtWriter lss)) of
          Left e -> assertFailure (show e)
          Right (out, ws) -> do
            assertBool "api_lang renamed" (any (\w -> warnWhere w == "api_lang") ws)
            case convert lss sjs out of
              Left e -> assertFailure (show e)
              Right (back, _) -> do
                let txt = TE.decodeUtf8 (BL.toStrict back)
                assertBool "french title kept" ("Avis client" `T.isInfixOf` txt)
                assertBool "logic follows the rename" ("{features} contains 'api'" `T.isInfixOf` txt && "\"apilang\"" `T.isInfixOf` txt)
    ]
  where
    readAs name path = do
      bs <- BL.readFile path
      let f = maybe (error name) id (lookupFormat name)
      either (assertFailure . show) pure (runConv (maybe (failWith "no reader") ($ bs) (fmtReader f)))
    firstQ s = case questions s of
      (q : _) -> Just q
      [] -> Nothing
    firstOf = \case
      (x : _) -> Just x
      [] -> Nothing
    readLimeSurveyTsv' = maybe (\_ -> failWith "no reader") id (lookupFormat "limesurvey-tsv" >>= fmtReader)

-- | Pre-ExpressionScript conditions: scenario 1 is car = Y and cars > 2,
-- scenario 2 is car = N; ids tie the SGQA field names to questions.
conditionsTsv :: BL.ByteString
conditionsTsv =
  BL.fromStrict . TE.encodeUtf8 . T.unlines . map (T.intercalate "\t") $
    [ ["id", "related_id", "class", "type/scale", "name", "relevance", "text", "language", "mandatory"]
    , ["", "", "S", "", "language", "", "en", "", ""]
    , ["", "", "S", "", "additional_languages", "", "de", "", ""]
    , ["", "", "SL", "", "surveyls_title", "", "Cars", "en", ""]
    , ["", "", "SL", "", "surveyls_title", "", "Autos", "de", ""]
    , ["1", "", "G", "1", "Cars", "", "", "en", ""]
    , ["10", "", "Q", "Y", "car", "", "Do you own a car?", "en", "Y"]
    , ["11", "", "Q", "N", "cars", "", "How many?", "en", ""]
    , ["1", "10", "C", "1", "123X1X10", "==", "Y", "", ""]
    , ["12", "", "Q", "T", "why", "", "\"Why?\"", "en", ""]
    , ["2", "10", "C", "1", "123X1X10", "==", "Y", "", ""]
    , ["3", "11", "C", "1", "123X1X11", ">", "2", "", ""]
    , ["4", "10", "C", "2", "123X1X10", "==", "N", "", ""]
    , ["1", "", "G", "1", "Autos", "", "", "de", ""]
    , ["10", "", "Q", "Y", "car", "", "Hast du ein Auto?", "de", "Y"]
    , ["11", "", "Q", "N", "cars", "", "Wie viele?", "de", ""]
    , ["12", "", "Q", "T", "why", "", "Warum?", "de", ""]
    ]
-- | Every feature either survives a format's round trip or is reported.
noSilentLoss :: FormatSpec -> TestTree
noSilentLoss f = testCase (fmtName f) $
  case [(featName ft, o) | ft <- features, Just w <- [fmtWriter f], let o = audit (fmtReader f) w ft, bad o] of
    [] -> pure ()
    xs -> assertFailure (unlines [T.unpack n <> ": " <> show o | (n, o) <- xs])
  where
    bad = \case
      Silent -> True
      Failed _ -> True
      _ -> False

simplifyTests :: TestTree
simplifyTests =
  testGroup
    "logic simplifier"
    [ testCase "value sets narrow" $
        simplify (EBin And (EBin Or (eq "s" "1") (eq "s" "2")) (EBin Neq (ERef "s") (ELit (LStr "1")))) @?= eq "s" "2"
    , testCase "constants fold" $ simplify (EBin And (ELit (LBool True)) (eq "a" "x")) @?= eq "a" "x"
    , testCase "contradiction is false" $ simplify (EBin And (eq "a" "x") (eq "a" "y")) @?= ELit (LBool False)
    , testCase "tidy logic is unchanged" $
        let e = EBin Or (EBin And (eq "a" "x") (ESelected "b" "y")) (ENot (EAnswered "c")) in simplify e @?= e
    ]
  where
    eq q v = EBin Eq (ERef q) (ELit (LStr v))

------------------------------------------------------------------------------
-- Microsoft Forms

msformsTests :: TestTree
msformsTests =
  testGroup
    "msforms"
    [ testCase "every question type" $ do
        s <- readFixture "all-types"
        let qs = questions s
            ty i = qType <$> lookupQ i qs
        length qs @?= 10
        ty "r0email" @?= Just (QText Email)
        ty "r0tools" @?= Just (QChoice Multiple Radio [choice "Forms" "Forms", choice "Excel" "Excel", Choice "Other" (plain "Other") True False])
        ty "r0rating" @?= Just (QScale 1 5 (Just (plain "Poor")) (Just (plain "Great")))
        ty "r0nps" @?= Just (QScale 0 10 (Just (plain "Not at all likely")) (Just (plain "Extremely likely")))
        ty "r0rank" @?= Just (QRanking [choice "Branching" "Branching", choice "Uploads" "Uploads"])
        ty "r0likert" @?= Just (QMatrix Single [choice "r0likert1" "Forms is useful", choice "r0likert2" "Forms is fast"] [choice l l | l <- ["Disagree", "Neutral", "Agree"]])
        ty "r0upload" @?= Just QFile
        ty "r0comments" @?= Just (QText LongText)
        [groupRelevant g | ItemGroup g <- formItems s] @?= [Nothing, Just notNo]
        (qRelevant =<< lookupQ "r0email" qs) @?= Just notNo
    , testCase "section jumps become show-ifs" $ do
        s <- readFixture "branching"
        [(groupId g, groupRelevant g) | ItemGroup g <- formItems s]
          @?= [("p1", Nothing), ("p2", Just keepPlace), ("p3", Just keepPlace)]
    , testCase "basic types" $ do
        s <- readFixture "basic-types"
        map qType (questions s) @?= [QText LongText, QChoice Single Radio [choice "I would ride it again" "I would ride it again", choice "Once was enough" "Once was enough"], QScale 1 5 Nothing Nothing, QDate Date]
    , testCase "live editor capture (2026-09)" $ do
        s <- readFixture "panform-capture-2026-09"
        let qs = questions s
            drive = EBin Neq (ERef "r9277a36c78c946b290e969354ef846b3") (ELit (LStr "No"))
        length qs @?= 2
        [(groupTitle g, groupRelevant g, map itemId (groupItems g)) | ItemGroup g <- formItems s]
          @?= [ (Nothing, Nothing, ["r9277a36c78c946b290e969354ef846b3"])
              , (Just (plain "Drivers"), Just drive, ["re4244132e08b4f38b4913b30f01c1b14"])
              ]
        map qRequired qs @?= [True, False]
        map qType qs @?= [QChoice Single Radio [choice "Yes" "Yes", choice "No" "No", Choice "Other" (plain "Other") True False], QText ShortText]
        map qRelevant qs @?= [Nothing, Nothing]
    , testCase "page show-if written as a jump" $ do
        let q1 = question "q1" "Drive?" (QChoice Single Radio [choice "Yes" "Yes", choice "No" "No"])
            s =
              Form
                emptyMeta
                [ ItemGroup (Group "p1" Page Nothing Nothing [ItemQuestion q1] M.empty)
                , ItemGroup (Group "p2" Page Nothing (Just (EBin Neq (ERef "q1") (ELit (LStr "No")))) [ItemQuestion (question "q2" "Car?" (QText ShortText))] M.empty)
                ]
        case runConv (msforms fmtWriter s) of
          Left e -> assertFailure (show e)
          Right (out, ws) -> do
            ws @?= []
            assertBool "ToTheEnd" ("ToTheEnd" `T.isInfixOf` T.pack (show out))
            fmap fst (runConv (msforms fmtReader out)) @?= Right s
    ]
  where
    notNo = EBin Neq (ERef "r0customer") (ELit (LStr "No"))
    keepPlace = EBin Neq (ERef "q1") (ELit (LStr "No, take me off the list"))
    lookupQ i qs = case filter ((== i) . qId) qs of
      (q : _) -> Just q
      [] -> Nothing
    msforms :: (FormatSpec -> Maybe a) -> a
    msforms f = maybe (error "msforms format missing") id (lookupFormat "msforms" >>= f)
    readFixture n = do
      bs <- BL.readFile ("examples/msforms/" <> n <> ".msforms.json")
      either (\e -> assertFailure (show e) >> error "unreachable") (pure . fst) (runConv (msforms fmtReader bs))

-- | Every example file, written in every format with a reader, reads back
-- to a survey whose second write is byte-identical to the first.
crossFixedPoints :: TestTree
crossFixedPoints = testCase "every example reaches a fixed point in every format" $ do
  files <- concat <$> mapM listExamples ["examples", "examples/limesurvey", "examples/msforms", "examples/gpl/limesurvey"]
  let rw = [f | f <- formats, Just _ <- [fmtReader f], Just _ <- [fmtWriter f]]
      failures =
        [ path <> " -> " <> fmtName to <> ": " <> why
        | (path, bytes) <- files
        , Just from <- [formatForPath path]
        , Just r <- [fmtReader from]
        , Right (s, _) <- [runConv (r bytes)]
        , to <- rw
        , Just why <- [check s to]
        ]
      check s to = case (fmtReader to, fmtWriter to) of
        (Just r, Just w) -> case runConv (w s) of
          Left _ -> Nothing -- a writer may refuse a survey outright
          Right (out1, _) -> case runConv (r out1 >>= w) of
            Left (ConvError e) -> Just ("cannot read own output: " <> T.unpack e)
            Right (out2, _)
              | out1 == out2 -> Nothing
              | otherwise ->
                  let n = length (takeWhile id (BL.zipWith (==) out1 out2))
                   in Just ("differs at byte " <> show n <> ": " <> show (BL.take 60 (BL.drop (fromIntegral n) out1)) <> " vs " <> show (BL.take 60 (BL.drop (fromIntegral n) out2)))
        _ -> Nothing
  case failures of
    [] -> pure ()
    xs -> assertFailure (unlines (take 40 xs) <> show (length xs) <> " failures")
  where
    listExamples dir = do
      ok <- doesDirectoryExist dir
      if not ok
        then pure []
        else do
          names <- listDirectory dir
          files <- filterM doesFileExist [dir </> n | n <- names, n /= "SOURCE.md"]
          mapM (\p -> (,) p <$> BL.readFile p) files

-- | Written back in its own format, an example reads back to the same form,
-- extension attributes included, unless the writer said what it changed.
sameFormatLossless :: TestTree
sameFormatLossless = testCase "every example survives its own format or warns" $ do
  files <- concat <$> mapM listExamples ["examples", "examples/limesurvey", "examples/msforms", "examples/gpl/limesurvey"]
  let failures =
        [ path <> " (" <> fmtName f <> "): changed without a warning"
        | (path, bytes) <- files
        , Just f <- [formatForPath path]
        , Just r <- [fmtReader f]
        , Just w <- [fmtWriter f]
        , Right (s0, _) <- [runConv (r bytes)]
        , Right (out, ws) <- [runConv (w s0)]
        , null ws
        , Right (s1, ws2) <- [runConv (r out)]
        , null ws2
        , s1 /= s0
        ]
  case failures of
    [] -> pure ()
    xs -> assertFailure (unlines xs)
  where
    listExamples dir = do
      ok <- doesDirectoryExist dir
      if not ok
        then pure []
        else do
          names <- listDirectory dir
          paths <- filterM doesFileExist [dir </> n | n <- names, n /= "SOURCE.md", n /= "SOURCES.md", not (".LICENSE" `isSuffixOf` n)]
          mapM (\p -> (,) p <$> BL.readFile p) paths

-- Five authored design scenarios per primary target, plus all-format loss
-- reporting. No vendor accounts, private corpus data, or network requests.
visualDesignTests :: TestTree
visualDesignTests = testGroup "visual design"
  [ testGroup "Jotform scenarios" [roundTrip "jotform" ("brand " <> show n) props | (n, props) <- zip [1 :: Int ..] jotformDesigns]
  , testGroup "Formbricks scenarios" [roundTrip "formbricks" ("brand " <> show n) (object ["styling" .= style]) | (n, style) <- zip [1 :: Int ..] formbricksDesigns]
  , roundTrip "surveyjs" "logo and responsive layout" (object ["logo" .= ("https://example.org/logo.png" :: T.Text), "logoWidth" .= (120 :: Int), "widthMode" .= ("responsive" :: T.Text), "questionsOnPageMode" .= ("questionPerPage" :: T.Text), "showProgressBar" .= False])
  , roundTrip "msforms" "stored background and header" (object ["otherInfo" .= ("{\"themeId\":\"custom\"}" :: T.Text), "background" .= object ["resourceUrl" .= ("https://example.org/bg.png" :: T.Text)], "header" .= Null])
  , testCase "every registered visual field survives, including null and empty values" $
      mapM_ (\name -> mapM_ (\value -> do
        let raw = object [(K.fromText field, value) | field <- designFields name]
            meta = emptyMeta {metaAttrs = readDesign name raw}
        object (writeDesign name meta) @?= raw) [Null, String "", Bool False, Number 0, object []]) ["jotform", "formbricks", "surveyjs", "msforms"]
  , testGroup "all destinations report unmapped designs"
      [ testCase (fmtName target <> " from " <> T.unpack source) $ do
          let keyName = if source == "typeform" then "typeform:theme" else source <> ":design"
              raw = if source == "typeform" then object ["href" .= ("https://api.typeform.com/themes/example" :: T.Text)] else object [(K.fromText field, String "custom") | field <- take 1 (designFields source)]
              form = Form emptyMeta {metaAttrs = M.singleton keyName raw} [ItemQuestion (question "why" "Why?" (QText LongText))]
          case runConv (writer target form) of
            Left _ -> pure () -- refusing a conversion is also explicit
            Right (_, ws) -> assertBool "design loss must be named" (any ((== "design") . warnWhere) ws)
      | source <- ["jotform", "formbricks", "surveyjs", "msforms", "typeform"]
      , target <- formats
      , fmtName target `notElem` ["native", "questionpunk", T.unpack source]
      ]
  , testGroup "lossless intermediate formats"
      [ testCase (name <> " carries " <> T.unpack source) $ do
          let props = object [(K.fromText field, String "  original  ") | field <- take 1 (designFields source)]
              form = Form emptyMeta {metaAttrs = readDesign source props} [ItemQuestion (question "q1" "Feedback" (QText LongText))]
              f = getFormat name
          (out, _) <- right (runConv (writer f form >>= reader f))
          metaAttrs (formMeta out) @?= metaAttrs (formMeta form)
      | name <- ["native", "questionpunk"], source <- ["jotform", "formbricks", "surveyjs", "msforms"]
      ]
  , testCase "catalog describes all 35 converters, including write-only outputs" $ do
      let entries = keyArr "formats" catalog
      length entries @?= length formats
      length entries @?= 35
      key "mode" catalog @?= Just (String "offline_conversion")
      map (key "id") entries @?= map (Just . String . T.pack . fmtName) formats
      mapM_ (\e -> assertBool "design capability has an explanation" ((key "design" e >>= key "note") /= Nothing)) entries
  ]
  where
    getFormat name = maybe (error ("missing format " <> name)) id (lookupFormat name)
    reader f = maybe (error "missing reader") id (fmtReader f)
    writer f = maybe (error "missing writer") id (fmtWriter f)
    right = either (\e -> assertFailure (show e) >> error "unreachable") pure
    roundTrip name title props = testCase title $ do
      let f = getFormat name
          source = T.pack name
          form = Form emptyMeta {metaAttrs = readDesign source props} [ItemQuestion (question "q1" "How was your visit?" (QText LongText))]
      (bytes, ws) <- right (runConv (writer f form))
      assertBool "no design loss on same-format write" (not (any ((== "design") . warnWhere) ws))
      (back, _) <- right (runConv (reader f bytes))
      metaAttrs (formMeta back) M.! (source <> ":design") @?= props
      (again, _) <- right (runConv (writer f back))
      again @?= bytes
      -- Exercise a real reader with native JSON, not only extension metadata.
      let payload = case decode bytes of
            Just (Object o) | name == "jotform" -> Object (KM.insert "properties" props o)
            Just (Object o) | Object visual <- props -> Object (KM.union visual o)
            _ -> error "writer emitted invalid JSON"
      (imported, _) <- right (runConv (reader f (encode payload)))
      metaAttrs (formMeta imported) M.! (source <> ":design") @?= props
    jotformDesigns =
      [ object ["font" .= ("Arial" :: T.Text), "fontcolor" .= ("#173B3F" :: T.Text), "background" .= ("#F5F9F7" :: T.Text)]
      , object ["font" .= ("Georgia" :: T.Text), "fontsize" .= ("18" :: T.Text), "styles" .= ("OldPaper" :: T.Text)]
      , object ["formWidth" .= ("640" :: T.Text), "alignment" .= ("Top" :: T.Text), "lineSpacing" .= ("16" :: T.Text)]
      , object ["background" .= ("#101827" :: T.Text), "fontcolor" .= ("#FFFFFF" :: T.Text), "highlightLine" .= ("Disabled" :: T.Text)]
      , object ["injectCSS" .= ("/* preserve exactly */\n.form-label { color: #174a50; }\n" :: T.Text), "labelWidth" .= ("140" :: T.Text)]
      ]
    formbricksDesigns =
      [ object ["brandColor" .= object ["light" .= ("#174A50" :: T.Text), "dark" .= ("#A0E0D0" :: T.Text)]]
      , object ["roundness" .= (12 :: Int), "allowStyleOverwrite" .= True]
      , object ["cardBackgroundColor" .= object ["light" .= ("#F8F5ED" :: T.Text), "dark" .= ("#191815" :: T.Text)]]
      , object ["background" .= object ["bg" .= ("https://example.org/fieldwork.jpg" :: T.Text), "bgType" .= ("image" :: T.Text)]]
      , object ["brandColor" .= object ["light" .= ("#002B55" :: T.Text)], "futureVendorProperty" .= object ["keep" .= True]]
      ]
