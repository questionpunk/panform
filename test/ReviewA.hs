-- | Regression tests for the pre-publish review: hostile input must end in
-- an error or a warning (never an exception, a hang or runaway memory), and
-- nothing is dropped or changed without a warning.
module ReviewA (tests) where

import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Csv (parseCells, readRows)
import Panform.Format.Codebook (maxScalePoints, scaleCodes, scaleTooWide)
import Panform.Format.TextUtil (scaleChoices)
import Panform.Formats
import Panform.Json (asText)
import Panform.Xlsx (readXlsx, writeXlsx)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  -- a regression in one of the bounds shows as a failure, not a hung run
  localOption (mkTimeout 120000000) $
    testGroup
      "review fixes"
      [ qualtricsEntityTests
      , jsonTests
      , xlsxTests
      , ddiGroupTests
      , odmTests
      , qtiBranchTests
      , scaleTests
      , qualtricsTests
      , qualtricsTxtTests
      , odmBoundTests
      , xmlEncodingTests
      , csvTests
      ]

------------------------------------------------------------------------------
-- Helpers

fmt :: String -> FormatSpec
fmt n = maybe (error ("no format " <> n)) id (lookupFormat n)

readAs :: String -> BL.ByteString -> Either ConvError (Form, [Warning])
readAs n bs = runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n)))

writeAs :: String -> Form -> Either ConvError (BL.ByteString, [Warning])
writeAs n s = runConv (maybe (failWith "no writer") ($ s) (fmtWriter (fmt n)))

-- | Read, failing the test on an error.
reads' :: String -> BL.ByteString -> IO (Form, [Warning])
reads' n bs = either (\e -> assertFailure (show e) >> error "unreachable") pure (readAs n bs)

writes' :: String -> Form -> IO (BL.ByteString, [Warning])
writes' n s = either (\e -> assertFailure (show e) >> error "unreachable") pure (writeAs n s)

utf8 :: Text -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8

text :: BL.ByteString -> Text
text = TE.decodeUtf8 . BL.toStrict

warned :: Text -> [Warning] -> Bool
warned what = any ((what `T.isInfixOf`) . warnWhat)

findQ :: Form -> Text -> Maybe Question
findQ s i = case [q | q <- questions s, qId q == i] of
  (q : _) -> Just q
  [] -> Nothing

form :: [Item] -> Form
form = Form emptyMeta

-- | A zip archive of entries given as (path, compression method, bytes as
-- stored). Method 0 is "stored"; 8 is deflate, for handing the reader a
-- stream that is not valid deflate.
zipOf :: [(String, Int, BL.ByteString)] -> BL.ByteString
zipOf entries = B.toLazyByteString (mconcat (map fst locals) <> central <> end)
  where
    locals = go 0 entries
    go _ [] = []
    go off ((name, method, body) : rest) =
      let n = BL8.pack name
          header =
            B.word32LE 0x04034b50
              <> B.word16LE 20
              <> B.word16LE 0
              <> B.word16LE (fromIntegral method)
              <> B.word16LE 0
              <> B.word16LE 0
              <> B.word32LE 0
              <> B.word32LE (fromIntegral (BL.length body))
              <> B.word32LE (fromIntegral (BL.length body))
              <> B.word16LE (fromIntegral (BL.length n))
              <> B.word16LE 0
              <> B.lazyByteString n
              <> B.lazyByteString body
          size = 30 + BL.length n + BL.length body
          dir =
            B.word32LE 0x02014b50
              <> B.word16LE 20
              <> B.word16LE 20
              <> B.word16LE 0
              <> B.word16LE (fromIntegral method)
              <> B.word16LE 0
              <> B.word16LE 0
              <> B.word32LE 0
              <> B.word32LE (fromIntegral (BL.length body))
              <> B.word32LE (fromIntegral (BL.length body))
              <> B.word16LE (fromIntegral (BL.length n))
              <> B.word16LE 0
              <> B.word16LE 0
              <> B.word16LE 0
              <> B.word16LE 0
              <> B.word32LE 0
              <> B.word32LE (fromIntegral off)
              <> B.lazyByteString n
       in (header, (dir, 46 + BL.length n)) : go (off + size) rest
    localsSize = sum [30 + fromIntegral (length name) + BL.length body | (name, _, body) <- entries]
    central = mconcat (map (fst . snd) locals)
    centralSize = sum (map (snd . snd) locals)
    end =
      B.word32LE 0x06054b50
        <> B.word16LE 0
        <> B.word16LE 0
        <> B.word16LE (fromIntegral (length entries))
        <> B.word16LE (fromIntegral (length entries))
        <> B.word32LE (fromIntegral centralSize)
        <> B.word32LE (fromIntegral localsSize)
        <> B.word16LE 0

-- | A one-sheet workbook around the given @sheetData@ content, with
-- optional shared strings.
workbook :: Maybe Text -> Text -> BL.ByteString
workbook shared rows =
  zipOf $
    [ ("xl/workbook.xml", 0, utf8 "<workbook xmlns:r=\"r\"><sheets><sheet name=\"survey\" sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>")
    , ("xl/_rels/workbook.xml.rels", 0, utf8 "<Relationships><Relationship Id=\"rId1\" Target=\"worksheets/sheet1.xml\"/></Relationships>")
    , ("xl/worksheets/sheet1.xml", 0, utf8 ("<worksheet><sheetData>" <> rows <> "</sheetData></worksheet>"))
    ]
      ++ [("xl/sharedStrings.xml", 0, utf8 s) | Just s <- [shared]]

inline :: Text -> Text -> Text
inline ref v = "<c" <> (if T.null ref then "" else " r=\"" <> ref <> "\"") <> " t=\"inlineStr\"><is><t>" <> v <> "</t></is></c>"

------------------------------------------------------------------------------
-- Qualtrics: numeric character references

qualtricsEntityTests :: TestTree
qualtricsEntityTests =
  testGroup
    "qsf entities"
    [ testCase "a reference outside Unicode stays as text instead of crashing" $ do
        (s, _) <- reads' "qsf" (qsf "bad &#x110000; and &#9999999; and &#xD800; ok &#x41; &#65; &#x41zz;")
        fmap (i18nDefault . qLabel) (findQ s "Q1") @?= Just "bad &#x110000; and &#9999999; and &#xD800; ok A A &#x41zz;"
    ]

-- | A one-question QSF with the given question text.
qsf :: Text -> BL.ByteString
qsf label =
  utf8 $
    "{\"SurveyEntry\":{\"SurveyID\":\"SV_1\",\"SurveyName\":\"t\",\"SurveyLanguage\":\"EN\"},\"SurveyElements\":["
      <> "{\"Element\":\"BL\",\"Payload\":[{\"Type\":\"Default\",\"ID\":\"BL_1\",\"BlockElements\":[{\"Type\":\"Question\",\"QuestionID\":\"QID1\"}]}]},"
      <> "{\"Element\":\"FL\",\"Payload\":{\"Flow\":[{\"Type\":\"Block\",\"ID\":\"BL_1\",\"FlowID\":\"FL_2\"}]}},"
      <> "{\"Element\":\"SQ\",\"PrimaryAttribute\":\"QID1\",\"Payload\":{\"QuestionID\":\"QID1\",\"DataExportTag\":\"Q1\",\"QuestionType\":\"TE\",\"Selector\":\"SL\",\"QuestionText\":\""
      <> label
      <> "\"}}]}"

------------------------------------------------------------------------------
-- JSON numbers

jsonTests :: TestTree
jsonTests =
  testGroup
    "json numbers"
    [ testCase "a huge exponent is not written out digit by digit" $
        (decode "1e5000" >>= asText) @?= Just "1.0e5000"
    , testCase "ordinary numbers read as before" $ do
        (decode "12" >>= asText) @?= Just "12"
        (decode "1e3" >>= asText) @?= Just "1000"
        (decode "1.5" >>= asText) @?= Just "1.5"
        asText (String "x") @?= Just "x"
    ]

------------------------------------------------------------------------------
-- XLSX

xlsxTests :: TestTree
xlsxTests =
  testGroup
    "xlsx"
    [ testCase "a column past XFD is refused, not built" $ do
        assertBool "ZZZZZZ1" (isLeft (readXlsx (workbook Nothing ("<row>" <> inline "ZZZZZZ1" "x" <> "</row>"))))
        assertBool "XFE1" (isLeft (readXlsx (workbook Nothing ("<row>" <> inline "XFE1" "x" <> "</row>"))))
        fmap (map (map length . snd)) (readXlsx (workbook Nothing ("<row>" <> inline "XFD1" "x" <> "</row>"))) @?= Right [[16384]]
    , testCase "a malformed cell reference is refused" $
        assertBool "b7" (isLeft (readXlsx (workbook Nothing ("<row>" <> inline "b7" "x" <> "</row>"))))
    , testCase "too many cells in all are refused" $
        assertBool "400 rows ending at XFD" (isLeft (readXlsx (workbook Nothing (T.replicate 400 ("<row>" <> inline "XFD1" "x" <> "</row>")))))
    , testCase "cells without a reference follow the cell before them" $ do
        readXlsx (workbook Nothing ("<row>" <> inline "" "a" <> inline "" "b" <> "</row><row>" <> inline "C2" "c" <> inline "" "d" <> "</row>"))
          @?= Right [("survey", [["a", "b"], ["", "", "c", "d"]])]
    , testCase "phonetic runs are not part of a string" $ do
        let shared = "<sst><si><t>東京</t><rPh sb=\"0\" eb=\"2\"><t>トウキョウ</t></rPh><phoneticPr fontId=\"1\"/></si><si><r><t>rich </t></r><r><t>text</t></r></si></sst>"
        readXlsx (workbook (Just shared) "<row><c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" t=\"s\"><v>1</v></c></row>")
          @?= Right [("survey", [["東京", "rich text"]])]
    , testCase "text that looks like an _xHHHH_ escape survives a round trip" $ do
        let cells = [["_x0041_", "a_x000a_b", "_x005F_", "_x0041_x0042_", "plain_x", "line\nbreak\ttab\rcr\SOHctl"]]
        readXlsx (writeXlsx [("survey", cells)]) @?= Right [("survey", cells)]
    , testCase "a part that inflates past the limit is refused" $ do
        -- 33 MiB of one letter deflates to a few kilobytes
        let bomb = writeXlsx [("survey", [[T.replicate (33 * 1024 * 1024) "a"]])]
        assertBool "the archive itself is small" (BL.length bomb < 200000)
        case readXlsx bomb of
          Left e -> assertBool e ("too large" `T.isInfixOf` T.pack e)
          Right _ -> assertFailure "read a 33 MiB sheet"
    , testCase "a corrupt deflate stream is an error, not an exception" $ do
        let bad =
              zipOf
                [ ("xl/workbook.xml", 8, "this is not a deflate stream")
                , ("xl/_rels/workbook.xml.rels", 0, "<Relationships/>")
                ]
        case readXlsx bad of
          Left e -> assertBool e ("cannot decompress" `T.isInfixOf` T.pack e)
          Right _ -> assertFailure "read a corrupt archive"
    , testCase "the xlsform reader reports these as conversion errors" $
        case readAs "xlsform" (workbook Nothing ("<row>" <> inline "ZZZZZZ1" "x" <> "</row>")) of
          Left (ConvError e) -> assertBool (T.unpack e) ("outside the sheet" `T.isInfixOf` e)
          Right _ -> assertFailure "read a cell in column ZZZZZZ"
    ]

------------------------------------------------------------------------------
-- DDI variable groups

ddiGroupTests :: TestTree
ddiGroupTests =
  testGroup
    "ddi variable groups"
    [ testCase "groups that share children are placed once each" $ do
        -- 40 levels of two groups, each listing both groups of the next
        -- level: 2^40 paths, 81 groups
        let depth = 40 :: Int
            name :: Text -> Int -> Text
            name side k = side <> T.pack (show k)
            level k =
              [ "<varGrp ID=\"" <> name side k <> "\" type=\"section\"" <> (if k < depth then " varGrp=\"" <> name "A" (k + 1) <> " " <> name "B" (k + 1) <> "\"" else " var=\"V" <> side <> "\"") <> "/>"
              | side <- ["A", "B"]
              ]
            doc = ddi (["<varGrp ID=\"root\" type=\"section\" varGrp=\"A1 B1\"/>"] ++ concatMap level [1 .. depth] ++ [var "VA", var "VB"])
        (s, ws) <- reads' "ddi" doc
        length [() | ItemGroup _ <- allItems (formItems s)] @?= 2 * depth + 1
        map qId (questions s) @?= ["VA", "VB"]
        assertBool "warned about the repeated listing" (warned "listed more than once" ws)
    , testCase "groups that list each other keep their variables" $ do
        let doc =
              ddi
                [ "<varGrp ID=\"A\" type=\"section\" var=\"V1\" varGrp=\"B\"/>"
                , "<varGrp ID=\"B\" type=\"section\" var=\"V2\" varGrp=\"A\"/>"
                , var "V1"
                , var "V2"
                , var "V3"
                ]
        (s, ws) <- reads' "ddi" doc
        map qId (questions s) @?= ["V1", "V2", "V3"]
        assertBool "warned about the loop" (warned "loop" ws)
    , testCase "a tree of groups reads as before, without warnings" $ do
        let doc =
              ddi
                [ "<varGrp ID=\"A\" type=\"section\" var=\"V1\" varGrp=\"B\"/>"
                , "<varGrp ID=\"B\" type=\"display\" var=\"V2\"/>"
                , var "V1"
                , var "V2"
                ]
        (s, ws) <- reads' "ddi" doc
        ws @?= []
        case formItems s of
          [ItemGroup a] -> map itemId (groupItems a) @?= ["V1", "B"]
          other -> assertFailure (show other)
    ]
  where
    ddi body = utf8 ("<codeBook><dataDscr>" <> T.concat body <> "</dataDscr></codeBook>")
    var n = "<var ID=\"" <> n <> "\" name=\"" <> n <> "\"><qstn><qstnLit>" <> n <> "?</qstnLit></qstn></var>"

------------------------------------------------------------------------------
-- ODM

odmTests :: TestTree
odmTests =
  testGroup
    "odm"
    [ testCase "ODM 2.0 groups that share children stop at the expansion limit" $ do
        let depth = 40 :: Int
            name :: Text -> Int -> Text
            name side k = side <> T.pack (show k)
            refsTo k = T.concat ["<ItemGroupRef ItemGroupOID=\"" <> name side k <> "\"/>" | side <- ["A", "B"]]
            level k =
              [ "<ItemGroupDef OID=\"" <> name side k <> "\" Name=\"g\">" <> (if k < depth then refsTo (k + 1) else "<ItemRef ItemOID=\"I1\"/>") <> "</ItemGroupDef>"
              | side <- ["A", "B"]
              ]
            doc = odm (["<ItemGroupDef OID=\"F\" Name=\"form\" Type=\"Form\">" <> refsTo 1 <> "</ItemGroupDef>"] ++ concatMap level [1 .. depth] ++ [itemDef "I1"])
        case readAs "odm" doc of
          Left (ConvError e) -> assertBool (T.unpack e) ("expand" `T.isInfixOf` e)
          Right _ -> assertFailure "expanded 2^40 groups"
    , testCase "ODM 2.0 group reused in two forms is read in both" $ do
        let doc =
              odm
                [ "<ItemGroupDef OID=\"F1\" Name=\"one\" Type=\"Form\"><ItemGroupRef ItemGroupOID=\"G\"/></ItemGroupDef>"
                , "<ItemGroupDef OID=\"F2\" Name=\"two\" Type=\"Form\"><ItemGroupRef ItemGroupOID=\"G\"/></ItemGroupDef>"
                , "<ItemGroupDef OID=\"G\" Name=\"shared\"><ItemRef ItemOID=\"I1\"/></ItemGroupDef>"
                , itemDef "I1"
                ]
        (s, _) <- reads' "odm" doc
        map qId (questions s) @?= ["I1", "I1"]
    , testCase "a matrix row's item id does not take another question's id" $ do
        let matrix = ItemQuestion (question "m" "Rate" (QMatrix Single [choice "r" "Row r", choice "s" "Row s"] [choice "1" "Yes", choice "2" "No"]))
            other = ItemQuestion (question "m_r" "Something else" (QText ShortText))
        check [matrix, other] ["m", "m_r_2"]
        check [other, matrix] ["m_r", "m"]
    , testCase "a checkbox option's item id does not take another question's id" $ do
        let multi = ItemQuestion (question "c" "Pick" (QChoice Multiple Radio [choice "a" "A", choice "b" "B"]))
            other = ItemQuestion (question "c___a" "Something else" (QText ShortText))
        check [multi, other] ["c", "c___a_2"]
        check [other, multi] ["c___a", "c"]
    , testCase "repeated ids are still numbered in order" $ do
        let q = ItemQuestion (question "x" "X" (QText ShortText))
            multi = ItemQuestion (question "c" "Pick" (QChoice Multiple Radio [choice "a" "A", choice "a" "B", choice "a_2" "C", choice "a" "D"]))
        (out, _) <- writes' "odm" (form [ItemGroup (Group "p" Page Nothing Nothing [q, q, q, multi] M.empty)])
        (s, _) <- reads' "odm" out
        map qId (questions s) @?= ["x", "x_2", "x_3", "c"]
        fmap qType (findQ s "c") @?= Just (QChoice Multiple Radio [choice "a" "A", choice "a_2" "B", choice "a_2_2" "C", choice "a_3" "D"])
    ]
  where
    odm body = utf8 ("<ODM ODMVersion=\"2.0\"><Study OID=\"S\"><MetaDataVersion OID=\"M\">" <> T.concat body <> "</MetaDataVersion></Study></ODM>")
    itemDef n = "<ItemDef OID=\"" <> n <> "\" Name=\"" <> n <> "\" DataType=\"text\"><Question><TranslatedText>" <> n <> "?</TranslatedText></Question></ItemDef>"
    -- written ids are all distinct, and reading back gives these questions
    check items expected = do
      (out, ws) <- writes' "odm" (form [ItemGroup (Group "p" Page Nothing Nothing items M.empty)])
      let oids = [T.takeWhile (/= '"') rest | chunk <- drop 1 (T.splitOn "<ItemDef OID=\"" (text out)), let rest = chunk]
      assertBool ("duplicate ItemDef OIDs in " <> show oids) (length oids == M.size (M.fromList [(o, ()) | o <- oids]))
      assertBool "the rename is reported" (warned "written as" ws)
      (s, _) <- reads' "odm" out
      map qId (questions s) @?= expected
      length [() | q <- questions s, QMatrix {} <- [qType q]] + length [() | q <- questions s, QChoice Multiple _ _ <- [qType q]] @?= 1
      -- and the output is a fixed point
      (again, _) <- writes' "odm" s
      again @?= out

------------------------------------------------------------------------------
-- QTI branch rules

qtiBranchTests :: TestTree
qtiBranchTests =
  testGroup
    "qti branch rules"
    [ testCase "a jump from an item that was not shown is not taken" $ do
        -- a is shown only when q0 = x, and always leaves the section
        (s, _) <- reads' "qti" (test' [item "q0" [] [], item "a" [q0is "x"] [("EXIT_SECTION", always)], item "b" [] []])
        rel s "a" @?= Just (q0 `eq` "x")
        rel s "b" @?= Just (q0 `neq` "x")
    , testCase "only the first branch rule that holds is taken" $ do
        -- from a: to c when q0 = x, otherwise out of the section
        (s, _) <- reads' "qti" (test' [item "q0" [] [], item "a" [] [("c", q0is "x"), ("EXIT_SECTION", always)], item "b" [] [], item "c" [] [], item "d" [] []])
        rel s "c" @?= Just (q0 `eq` "x")
        rel s "d" @?= Just (q0 `eq` "x")
    , testCase "a jump from an item another jump passed over is not taken" $ do
        -- a jumps to c when q0 = x, passing over b; b always jumps to e
        (s, _) <- reads' "qti" (test' [item "q0" [] [], item "a" [] [("c", q0is "x")], item "b" [] [("e", always)], item "c" [] [], item "d" [] [], item "e" [] []])
        rel s "b" @?= Just (q0 `neq` "x")
        rel s "c" @?= Just (q0 `eq` "x")
        rel s "d" @?= Just (q0 `eq` "x")
        rel s "e" @?= Nothing
    , testCase "a plain forward jump reads as before" $ do
        (s, ws) <- reads' "qti" (test' [item "q0" [] [("b", q0is "x")], item "a" [] [], item "b" [] []])
        rel s "a" @?= Just (q0 `neq` "x")
        rel s "b" @?= Nothing
        ws @?= []
    , testCase "backward and condition-less branch rules are reported" $ do
        (s, ws) <- reads' "qti" (test' [item "q0" [] [], item "a" [] [("q0", q0is "x"), ("EXIT_SECTION", "")], item "b" [] []])
        rel s "b" @?= Nothing
        assertBool "backward" (warned "backward branch to q0 dropped" ws)
        assertBool "no condition" (warned "it has no condition" ws)
    , testCase "branch rules that expand past the limit are refused" $
        -- 2000 items that can each leave the section: two million terms
        case readAs "qti" (test' (item "q0" [] [] : [item ("i" <> T.pack (show n)) [] [("EXIT_SECTION", q0is (T.pack (show n)))] | n <- [1 :: Int .. 2000]])) of
          Left (ConvError e) -> assertBool (T.unpack e) ("branch rules expand" `T.isInfixOf` e)
          Right _ -> assertFailure "expanded two million condition terms"
    , testCase "many exits within the limit are converted" $ do
        (s, _) <- reads' "qti" (test' (item "q0" [] [] : [item ("i" <> T.pack (show n)) [] [("EXIT_SECTION", q0is (T.pack (show n)))] | n <- [1 :: Int .. 300]]))
        rel s "i2" @?= Just (q0 `neq` "1")
        fmap (length . conjuncts) (rel s "i300") @?= Just 299
    ]
  where
    q0 = ERef "q0"
    eq a b = EBin Eq a (ELit (LStr b))
    neq a b = EBin Neq a (ELit (LStr b))
    rel s i = findQ s i >>= qRelevant
    conjuncts = \case
      EBin And a b -> conjuncts a ++ conjuncts b
      e -> [e]
    always = "<baseValue baseType=\"boolean\">true</baseValue>"
    q0is v = "<match><variable identifier=\"q0.RESPONSE\"/><baseValue baseType=\"string\">" <> v <> "</baseValue></match>"
    item :: Text -> [Text] -> [(Text, Text)] -> Text
    item i pres rules =
      "<assessmentItemRef identifier=\"" <> i <> "\" href=\"" <> i <> ".xml\">"
        <> T.concat ["<preCondition>" <> p <> "</preCondition>" | p <- pres]
        <> T.concat ["<branchRule target=\"" <> t <> "\">" <> c <> "</branchRule>" | (t, c) <- rules]
        <> "<assessmentItem identifier=\""
        <> i
        <> "\" title=\""
        <> i
        <> "\"><responseDeclaration identifier=\"RESPONSE\" cardinality=\"single\" baseType=\"string\"/><itemBody><p>"
        <> i
        <> "</p><p><textEntryInteraction responseIdentifier=\"RESPONSE\"/></p></itemBody></assessmentItem></assessmentItemRef>"
    test' items = utf8 ("<assessmentTest identifier=\"t\" title=\"t\"><testPart identifier=\"p\"><assessmentSection identifier=\"s\" title=\"s\" visible=\"true\">" <> T.concat items <> "</assessmentSection></testPart></assessmentTest>")

------------------------------------------------------------------------------
-- Scales written out point by point

scaleTests :: TestTree
scaleTests =
  testGroup
    "scale points"
    [ testCase "the shared helpers list at most maxScalePoints points" $ do
        length (take (maxScalePoints + 1) (scaleCodes 0 2000000000 Nothing Nothing)) @?= maxScalePoints
        length (take (maxScalePoints + 1) (scaleChoices 0 2000000000 Nothing Nothing)) @?= maxScalePoints
        length (take (maxScalePoints + 1) (scaleCodes minBound maxBound Nothing Nothing)) @?= maxScalePoints
        scaleTooWide 0 2000000000 @?= True
        scaleTooWide minBound maxBound @?= True
        scaleTooWide 0 (maxScalePoints - 1) @?= False
    , testCase "ordinary scales are listed in full" $ do
        map fst (scaleCodes 1 5 (Just (plain "low")) (Just (plain "high"))) @?= ["1", "2", "3", "4", "5"]
        map (i18nDefault . snd) (scaleCodes 1 5 (Just (plain "low")) (Just (plain "high"))) @?= ["low", "2", "3", "4", "high"]
        map (i18nDefault . choiceLabel) (scaleChoices 1 3 (Just (plain "low")) (Just (plain "high"))) @?= ["1 (low)", "2", "3 (high)"]
        length (scaleCodes 0 (maxScalePoints - 1) Nothing Nothing) @?= maxScalePoints
    , testCase "ddi warns when a scale is cut short" $ do
        (_, ws) <- writes' "ddi" (form [ItemQuestion (question "s" "S" (QScale 0 50000 Nothing Nothing))])
        assertBool "warned" (warned "first 10000 points" ws)
        (_, ws') <- writes' "ddi" (form [ItemQuestion (question "s" "S" (QScale 0 100 Nothing Nothing))])
        assertBool "no warning for an ordinary scale" (not (warned "points" ws'))
    , testCase "qualtrics-txt warns when a scale is cut short" $ do
        (out, ws) <- writes' "qualtrics-txt" (form [ItemQuestion (question "s" "S" (QScale 0 50000 Nothing Nothing))])
        assertBool "warned" (warned "first 10000 points" ws)
        assertBool "bounded output" (length (T.lines (text out)) < 2 * maxScalePoints + 20)
    , testCase "qsf: only a 0 to 10 scale is NPS, whatever the attribute says" $ do
        let nps = (question "s" "S" (QScale 0 50000 Nothing Nothing)) {qAttrs = M.singleton "qualtrics:selector" (String "NPS")}
            dep = (question "t" "T" (QText ShortText)) {qRelevant = Just (EBin Eq (ERef "s") (ELit (LNum 5)))}
        (out, _) <- writes' "qsf" (form [ItemGroup (Group "p" Page Nothing Nothing [ItemQuestion nps, ItemQuestion dep] M.empty)])
        assertBool "logic addresses the slider's value" ("ChoiceNumericEntryValue" `T.isInfixOf` text out)
        assertBool "not a choice of an NPS question" (not ("SelectableChoice" `T.isInfixOf` text out))
    ]

------------------------------------------------------------------------------
-- Qualtrics QSF

-- | A one-block QSF holding the given question payloads (JSON members
-- without braces, one per question) and extra survey elements.
qsfDoc :: [Text] -> [Text] -> BL.ByteString
qsfDoc payloads extra =
  utf8 $
    "{\"SurveyEntry\":{\"SurveyID\":\"SV_1\",\"SurveyName\":\"t\",\"SurveyLanguage\":\"EN\"},\"SurveyElements\":["
      <> "{\"Element\":\"BL\",\"Payload\":[{\"Type\":\"Default\",\"ID\":\"BL_1\",\"BlockElements\":["
      <> T.intercalate "," ["{\"Type\":\"Question\",\"QuestionID\":\"QID" <> n <> "\"}" | n <- nums]
      <> "]}]},"
      <> "{\"Element\":\"FL\",\"Payload\":{\"Flow\":[{\"Type\":\"Block\",\"ID\":\"BL_1\",\"FlowID\":\"FL_2\"}]}}"
      <> T.concat [",{\"Element\":\"SQ\",\"PrimaryAttribute\":\"QID" <> n <> "\",\"Payload\":{\"QuestionID\":\"QID" <> n <> "\"," <> pay <> "}}" | (n, pay) <- zip nums payloads]
      <> T.concat ["," <> e | e <- extra]
      <> "]}"
  where
    nums = map (T.pack . show) [1 .. length payloads]

qualtricsTests :: TestTree
qualtricsTests =
  testGroup
    "qsf"
    [ testCase "every spelling of a line break in plain text is a line break" $ do
        (s, ws) <- reads' "qsf" (qsf "one<br />two<BR>three<br/>four<br>five")
        fmap (i18nDefault . qLabel) (findQ s "Q1") @?= Just "one\ntwo\nthree\nfour\nfive"
        assertBool "plain text is not reported as markup" (not (warned "html markup" ws))
        -- written back as the plain line breaks it reads
        (out, _) <- writes' "qsf" s
        (s', _) <- reads' "qsf" out
        fmap (i18nDefault . qLabel) (findQ s' "Q1") @?= Just "one\ntwo\nthree\nfour\nfive"
    , testCase "an answered test on an NPS question reads back" $ do
        let nps = (question "n" "N" (QScale 0 10 Nothing Nothing)) {qAttrs = M.singleton "qualtrics:selector" (String "NPS")}
            yes = (question "a" "A" (QText ShortText)) {qRelevant = Just (EAnswered "n")}
            no = (question "b" "B" (QText ShortText)) {qRelevant = Just (ENot (EAnswered "n"))}
        (out, _) <- writes' "qsf" (form [ItemGroup (Group "p" Page Nothing Nothing (map ItemQuestion [nps, yes, no]) M.empty)])
        (s, ws) <- reads' "qsf" out
        (findQ s "a" >>= qRelevant) @?= Just (EAnswered "n")
        (findQ s "b" >>= qRelevant) @?= Just (ENot (EAnswered "n"))
        assertBool "no condition kept raw" (not (warned "kept in qualtrics form" ws))
    , testCase "dynamic choices and choice-count validation are reported" $ do
        let mc =
              "\"DataExportTag\":\"B2c\",\"QuestionType\":\"MC\",\"Selector\":\"MAVR\",\"QuestionText\":\"Which?\",\"Choices\":[],\"ChoiceOrder\":[],"
                <> "\"DynamicChoices\":{\"Type\":\"Dynamic\",\"DynamicType\":\"ChoiceGroup\",\"Locator\":\"q://QID9/ChoiceGroup/SelectedChoices\"},"
                <> "\"Validation\":{\"Settings\":{\"ForceResponse\":\"OFF\",\"Type\":\"MinChoices\",\"MinChoices\":\"2\"}}"
        (_, ws) <- reads' "qsf" (qsfDoc [mc] [])
        assertBool "dynamic choices" (warned "dynamic choices dropped (carried forward from q://QID9/ChoiceGroup/SelectedChoices)" ws)
        assertBool "validation" (warned "validation MinChoices dropped" ws)
    , testCase "a default answer or a validation the writer cannot place is reported" $ do
        let txt' = (question "t" "T" (QText ShortText)) {qDefault = Just "example"}
            scale = (question "s" "S" (QScale 1 5 Nothing Nothing)) {qValidation = Just (Validation (Just 2) Nothing Nothing Nothing)}
            rank = (question "r" "R" (QRanking [choice "1" "A", choice "2" "B"])) {qValidation = Just (Validation Nothing Nothing Nothing (Just (plain "pick")))}
        (_, ws) <- writes' "qsf" (form (map ItemQuestion [txt', scale, rank]))
        assertBool "default" (any (\w -> warnWhere w == "t" && warnWhat w == "default answer dropped") ws)
        assertBool "scale validation" (any (\w -> warnWhere w == "s" && warnWhat w == "validation dropped") ws)
        assertBool "ranking validation" (any (\w -> warnWhere w == "r" && warnWhat w == "validation dropped") ws)
    , testCase "survey protection and anonymity survive a round trip" $ do
        let so = "{\"Element\":\"SO\",\"PrimaryAttribute\":\"Survey Options\",\"Payload\":{\"SurveyProtection\":\"ByInvitation\",\"AnonymizeResponse\":\"Yes\",\"BallotBoxStuffingPrevention\":\"true\",\"RefererCheck\":\"Yes\",\"RefererURL\":\"https://example.org\",\"PasswordProtection\":\"No\",\"Password\":\"\"}}"
        (s, ws) <- reads' "qsf" (qsfDoc ["\"DataExportTag\":\"Q1\",\"QuestionType\":\"TE\",\"Selector\":\"SL\",\"QuestionText\":\"Hi\""] [so])
        assertBool "nothing to report" (not (warned "password" ws))
        (out, _) <- writes' "qsf" s
        let opts = case decode out >>= lookupKey "SurveyElements" of
              Just (Array els) -> [p | Object e <- foldr (:) [] els, Just (String "SO") <- [lookupO "Element" e], Just (Object p) <- [lookupO "Payload" e]]
              _ -> []
        case opts of
          [p] -> do
            lookupO "SurveyProtection" p @?= Just (String "ByInvitation")
            lookupO "AnonymizeResponse" p @?= Just (String "Yes")
            lookupO "BallotBoxStuffingPrevention" p @?= Just (String "true")
            lookupO "RefererCheck" p @?= Just (String "Yes")
            lookupO "RefererURL" p @?= Just (String "https://example.org")
          _ -> assertFailure "no survey options written"
        -- a survey without these settings is written as before
        (plainOut, _) <- writes' "qsf" (form [ItemQuestion (question "q" "Q" (QText ShortText))])
        assertBool "public by default" ("\"SurveyProtection\": \"PublicSurvey\"" `T.isInfixOf` text plainOut)
        assertBool "not anonymised by default" ("\"AnonymizeResponse\": \"No\"" `T.isInfixOf` text plainOut)
    , testCase "a survey password is reported, not copied" $ do
        let so = "{\"Element\":\"SO\",\"PrimaryAttribute\":\"Survey Options\",\"Payload\":{\"PasswordProtection\":\"Yes\",\"Password\":\"hunter2\"}}"
        (s, ws) <- reads' "qsf" (qsfDoc ["\"DataExportTag\":\"Q1\",\"QuestionType\":\"TE\",\"Selector\":\"SL\",\"QuestionText\":\"Hi\""] [so])
        assertBool "warned" (warned "survey password protection dropped" ws)
        assertBool "the password is not in the form" (not ("hunter2" `T.isInfixOf` T.pack (show s)))
    , testCase "text that refers to a question id is reported when written" $ do
        let piped = question "p" "You chose ${q://QID7/ChoiceGroup/SelectedChoices}" (QText ShortText)
            raw = (question "r" "R" (QText ShortText)) {qRelevant = Just (ERaw "qualtrics" "{\"LeftOperand\":\"q://QID7/SelectableChoice/1\"}")}
            clean = question "c" "Nothing to see" (QText ShortText)
        (_, ws) <- writes' "qsf" (form (map ItemQuestion [piped, raw, clean]))
        assertBool "piped text" (any (\w -> warnWhere w == "p" && "renumbered" `T.isInfixOf` warnWhat w) ws)
        assertBool "kept condition" (any (\w -> warnWhere w == "r" && "renumbered" `T.isInfixOf` warnWhat w) ws)
        assertBool "nothing for plain text" (not (any (\w -> warnWhere w == "c") ws))
    , testCase "a slider with several statements is kept whole" $ do
        let slider = "\"DataExportTag\":\"S\",\"QuestionType\":\"Slider\",\"Selector\":\"HSLIDER\",\"QuestionText\":\"Rate\",\"Choices\":{\"1\":{\"Display\":\"Taste\"},\"2\":{\"Display\":\"Price\"}},\"Configuration\":{\"CSSliderMin\":0,\"CSSliderMax\":10}"
        (s, ws) <- reads' "qsf" (qsfDoc [slider] [])
        fmap qType (findQ s "S") @?= Just (QOther "Slider")
        assertBool "kept by name" (warned "kept by name" ws)
    ]
  where
    lookupKey k = \case
      Object o -> lookupO k o
      _ -> Nothing
    lookupO k o = KM.lookup (K.fromText k) o

------------------------------------------------------------------------------
-- Qualtrics TXT

qualtricsTxtTests :: TestTree
qualtricsTxtTests =
  testGroup
    "qualtrics-txt"
    [ testCase "a line that reads as a tag stays text" $ do
        let q = question "q" "Before\n[[PageBreak]]\nafter" (QChoice Single Radio [choice "1" "[[Choices]]", choice "2" "  [[Question:Text]]", choice "3" "plain"])
        (out, ws) <- writes' "qualtrics-txt" (form [ItemGroup (Group "p" Page Nothing Nothing [ItemQuestion q] M.empty)])
        assertBool "reported" (warned "reads as a tag" ws)
        (s, _) <- reads' "qualtrics-txt" out
        length [() | ItemGroup _ <- formItems s] @?= 1
        fmap (i18nDefault . qLabel) (findQ s "q") @?= Just "Before\n[ [PageBreak]]\nafter"
        fmap qType (findQ s "q") @?= Just (QChoice Single Radio [choice "1" "[ [Choices]]", choice "2" "  [ [Question:Text]]", choice "3" "plain"])
        -- and it is a fixed point
        (again, _) <- writes' "qualtrics-txt" s
        again @?= out
    , testCase "whitespace the format cannot keep is reported" $ do
        (_, ws) <- writes' "qualtrics-txt" (form [ItemQuestion (question "q" "\nPadded  \n" (QText ShortText))])
        assertBool "warned" (warned "blank lines around text removed" ws)
        (_, ws') <- writes' "qualtrics-txt" (form [ItemQuestion (question "q" "Tidy\n\ntext" (QText ShortText))])
        assertBool "no warning for tidy text" (not (warned "removed" ws'))
        (_, ws'') <- writes' "qualtrics-txt" (form [ItemQuestion (question "q" "a\r\nb" (QText ShortText))])
        assertBool "carriage return" (warned "carriage returns" ws'')
    , testCase "a scale that runs downwards keeps its points" $ do
        (out, _) <- writes' "qualtrics-txt" (form [ItemQuestion (question "s" "S" (QScale 3 1 Nothing Nothing))])
        (s, _) <- reads' "qualtrics-txt" out
        fmap qType (findQ s "s") @?= Just (QChoice Single Radio [choice "3" "3", choice "2" "2", choice "1" "1"])
    ]

------------------------------------------------------------------------------
-- ODM range checks

odmBoundTests :: TestTree
odmBoundTests =
  testGroup
    "odm range checks"
    [ testCase "a strict bound on a whole number is the next value in" $ do
        (s, ws) <- reads' "odm" (doc "integer" [("GT", "1"), ("LT", "120")])
        (findQ s "I1" >>= qValidation) @?= Just (Validation (Just 2) (Just 119) Nothing Nothing)
        assertBool "exact, so nothing to report" (not (warned "range check" ws))
    , testCase "a strict bound on a decimal is reported, not silently made inclusive" $ do
        (s, ws) <- reads' "odm" (doc "float" [("GT", "1"), ("LT", "3")])
        (findQ s "I1" >>= qValidation) @?= Just (Validation (Just 1) (Just 3) Nothing Nothing)
        assertBool "GT" (warned "range check GT 1 read as at least 1" ws)
        assertBool "LT" (warned "range check LT 3 read as at most 3" ws)
    , testCase "inclusive bounds read as before, and a second bound is kept" $ do
        (s, ws) <- reads' "odm" (doc "float" [("GE", "1"), ("LE", "9"), ("GE", "2")])
        (findQ s "I1" >>= qValidation) @?= Just (Validation (Just 1) (Just 9) (Just (EBin Ge (ERef ".") (ELit (LNum 2)))) Nothing)
        ws @?= []
        (out, ws') <- writes' "odm" s
        assertBool "nothing dropped" (not (warned "validation" ws'))
        (s', _) <- reads' "odm" out
        (findQ s' "I1" >>= qValidation) @?= (findQ s "I1" >>= qValidation)
    , testCase "a strict rule on a whole number is written exactly" $ do
        let q = (question "n" "N" (QNumber Integer)) {qValidation = Just (Validation Nothing Nothing (Just (EBin Gt (ERef ".") (ELit (LNum 5)))) Nothing)}
        (out, ws) <- writes' "odm" (form [ItemGroup (Group "p" Page Nothing Nothing [ItemQuestion q] M.empty)])
        assertBool "nothing dropped" (not (warned "validation" ws))
        (s, _) <- reads' "odm" out
        (findQ s "n" >>= qValidation) @?= Just (Validation (Just 6) Nothing Nothing Nothing)
    , testCase "a strict rule on a decimal is reported as dropped" $ do
        let q = (question "d" "D" (QNumber Decimal)) {qValidation = Just (Validation Nothing Nothing (Just (EBin Gt (ERef ".") (ELit (LNum 1.5)))) Nothing)}
        (_, ws) <- writes' "odm" (form [ItemGroup (Group "p" Page Nothing Nothing [ItemQuestion q] M.empty)])
        assertBool "warned" (warned "validation expression dropped" ws)
    ]
  where
    doc :: Text -> [(Text, Text)] -> BL.ByteString
    doc dtype checks =
      utf8 $
        "<ODM ODMVersion=\"1.3.2\"><Study OID=\"S\"><MetaDataVersion OID=\"M\">"
          <> "<FormDef OID=\"F\" Name=\"f\"><ItemGroupRef ItemGroupOID=\"G\" Mandatory=\"No\"/></FormDef>"
          <> "<ItemGroupDef OID=\"G\" Name=\"g\"><ItemRef ItemOID=\"I1\" Mandatory=\"No\"/></ItemGroupDef>"
          <> "<ItemDef OID=\"I1\" Name=\"I1\" DataType=\""
          <> dtype
          <> "\"><Question><TranslatedText>How many?</TranslatedText></Question>"
          <> T.concat ["<RangeCheck Comparator=\"" <> c <> "\" SoftHard=\"Hard\"><CheckValue>" <> v <> "</CheckValue></RangeCheck>" | (c, v) <- checks]
          <> "</ItemDef></MetaDataVersion></Study></ODM>"

------------------------------------------------------------------------------
-- XML encodings

xmlEncodingTests :: TestTree
xmlEncodingTests =
  testGroup
    "xml encodings"
    [ testCase "windows-1252 punctuation reads as itself" $ do
        (s, _) <- reads' "ddi" (ddi "windows-1252" [0x80, 0x20, 0x93, 0x71, 0x94, 0x20, 0x97, 0x20, 0xE9, 0x20, 0x81])
        fmap (i18nDefault . qLabel) (findQ s "V1") @?= Just "\x20AC \x201Cq\x201D \x2014 \xE9 \x81"
    , testCase "latin-1 stays latin-1" $ do
        (s, _) <- reads' "ddi" (ddi "ISO-8859-1" [0x93, 0xE9])
        fmap (i18nDefault . qLabel) (findQ s "V1") @?= Just "\x93\xE9"
    , testCase "only the declaration names the encoding" $ do
        -- UTF-8 text that mentions latin1 near the top is still UTF-8
        let body = "<?xml version=\"1.0\"?><codeBook ID=\"latin1\"><dataDscr><var ID=\"V1\" name=\"V1\"><qstn><qstnLit>caf\xE9</qstnLit></qstn></var></dataDscr></codeBook>"
        (s, _) <- reads' "ddi" (utf8 body)
        fmap (i18nDefault . qLabel) (findQ s "V1") @?= Just "caf\xE9"
    ]
  where
    ddi enc bytes =
      utf8 ("<?xml version=\"1.0\" encoding=\"" <> enc <> "\"?><codeBook><dataDscr><var ID=\"V1\" name=\"V1\"><qstn><qstnLit>")
        <> BL.pack bytes
        <> utf8 "</qstnLit></qstn></var></dataDscr></codeBook>"

------------------------------------------------------------------------------
-- CSV

csvTests :: TestTree
csvTests =
  testGroup
    "csv"
    [ testCase "rows read as before" $
        readRows ',' "Name,Label\na,\"one, \"\"two\"\"\"\nb,three,\n" @?= Right [M.fromList [("name", "a"), ("label", "one, \"two\"")], M.fromList [("name", "b"), ("label", "three")]]
    , testCase "a header naming a column twice is refused" $
        assertBool "Name,name" (isLeft (readRows ',' "Name,name\na,b\n"))
    , testCase "a value past the last named column is refused" $
        assertBool "extra cell" (isLeft (readRows ',' "a,b\n1,2,3\n"))
    , testCase "a file that is not UTF-8 is refused" $
        assertBool "latin-1 byte" (isLeft (readRows ',' (BL.pack [0x61, 0x0A, 0xE9, 0x0A])))
    , testCase "quoted cells accumulate their own characters" $
        parseCells ',' "\"ab\"\"c\",d\n" @?= [["ab\"c", "d"]]
    ]
