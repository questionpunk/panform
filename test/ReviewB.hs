{-# LANGUAGE OverloadedStrings #-}

module ReviewB (tests) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Formats
import Panform.Jump
import System.Timeout (timeout)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "review fixes B"
    [ jumpTests
    , routingTests
    , limeSurveyTests
    , exprTests
    , formatTests
    ]

------------------------------------------------------------------------------
-- Helpers

fmt :: String -> FormatSpec
fmt n = fromMaybe (error ("no format " <> n)) (lookupFormat n)

readAs :: String -> BL.ByteString -> IO (Form, [Warning])
readAs n bs = case runConv (maybe (failWith "no reader") ($ bs) (fmtReader (fmt n))) of
  Left e -> assertFailure (show e)
  Right r -> pure r

writeAs :: String -> Form -> IO (BL.ByteString, [Warning])
writeAs n s = case runConv (maybe (failWith "no writer") ($ s) (fmtWriter (fmt n))) of
  Left e -> assertFailure (show e)
  Right r -> pure r

relOf :: Form -> T.Text -> Maybe Expr
relOf s i =
  case [r | it <- allItems (formItems s), itemId it == i, r <- [rel it]] of
    (r : _) -> r
    [] -> error ("no item " <> T.unpack i)
  where
    rel = \case
      ItemQuestion q -> qRelevant q
      ItemGroup g -> groupRelevant g
      ItemNote _ _ r -> r
      ItemRaw _ _ -> Nothing

decode' :: BL.ByteString -> Maybe Value
decode' = decode

keyText' :: KM.Key -> KM.KeyMap Value -> Maybe Value
keyText' = KM.lookup

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

isJust' :: Maybe a -> Bool
isJust' = maybe False (const True)

warned :: T.Text -> [Warning] -> Bool
warned what = any (T.isInfixOf what . warnWhat)

utf8 :: BL.ByteString -> T.Text
utf8 = TE.decodeUtf8 . BL.toStrict

_keep :: ()
_keep = const () (utf8, has, warned, writeAs)

has :: String -> BL.ByteString -> Bool
has needle bs = needle `isInfixOf` BLC.unpack bs

-- | Truth of a condition whose leaves are boolean references.
truth :: M.Map T.Text Bool -> Expr -> Bool
truth env = \case
  ELit (LBool b) -> b
  ERef v -> M.findWithDefault False v env
  ENot x -> not (truth env x)
  EBin And a b -> truth env a && truth env b
  EBin Or a b -> truth env a || truth env b
  e -> error ("not boolean: " <> show e)

------------------------------------------------------------------------------
-- Jump logic to show-if

-- | The screens a respondent sees: start at 0, on each screen take the
-- first rule that holds, else the fallback, else the next screen.
walk :: M.Map T.Text Bool -> [([Rule], Maybe Int)] -> [Int]
walk env units = go 0
  where
    n = length units
    go i
      | i >= n = []
      | otherwise =
          let (rs, fb) = units !! i
              next = case [ruleTarget r | r <- rs, truth env (ruleCond r)] of
                (k : _) -> k
                [] -> fromMaybe (i + 1) fb
           in i : go next

assignments :: [T.Text] -> [M.Map T.Text Bool]
assignments vs = map (M.fromList . zip vs) (mapM (const [False, True]) vs)

-- | Every screen's derived condition holds exactly when the respondent
-- reaches that screen, for every answer pattern.
agrees :: [T.Text] -> [([Rule], Maybe Int)] -> Assertion
agrees vs units = case runConv (jumpsToRelevance (T.pack . show) units) of
  Left e -> assertFailure (show e)
  Right (rels, _) -> forM_ (assignments vs) $ \env ->
    forM_ (zip [0 ..] rels) $ \(j, r) ->
      assertEqual
        ("screen " <> show (j :: Int) <> " under " <> show (M.toList env) <> " in " <> show units)
        (j `elem` walk env units)
        (maybe True (truth env) r)

-- | Deterministic pseudo-random forward-jump layouts.
layouts :: [[([Rule], Maybe Int)]]
layouts = take 600 (go 12345)
  where
    go :: Int -> [[([Rule], Maybe Int)]]
    go seed =
      let (units, seed') = layout seed
       in units : go seed'
    next s = (s * 1103515245 + 12345) `mod` 2147483648
    pick s k = (s `div` 65536) `mod` k
    layout s0 =
      let n = 6
          step (acc, s) i =
            let s1 = next s
                nRules = pick s1 3
                (rs, s2) = foldl (rule i) ([], s1) [1 .. nRules]
                s3 = next s2
                fb = if pick s3 3 == 0 then Just (i + 1 + pick (next s3) (n - i)) else Nothing
             in (acc ++ [(reverse rs, fb)], next s3)
          rule i (rs, s) _ =
            let s1 = next s
                s2 = next s1
                s3 = next s2
                var = ["a", "b", "c"] !! pick s1 3
                c = if pick s2 4 == 0 then ENot (ERef var) else ERef var
             in (Rule c (i + 1 + pick s3 (n - i)) : rs, s3)
       in foldl step ([], s0) [0 .. n - 1]

jumpTests :: TestTree
jumpTests =
  testGroup
    "jumps to show-if"
    [ testCase "a jump from a skipped screen does not fire" $ do
        -- Screen 0 always jumps to 2, so screen 1 (which would jump to 3)
        -- is never reached and screen 2 is shown.
        let units = [([Rule (ELit (LBool True)) 2], Nothing), ([Rule (ELit (LBool True)) 3], Nothing), ([], Nothing), ([], Nothing)]
        agrees [] units
    , testCase "a conditional jump from a conditionally skipped screen" $ do
        -- q1 "a -> q3", q2 "b -> q4": with a, the flow is q1, q3, q4.
        let units = [([Rule (ERef "a") 2], Nothing), ([Rule (ERef "b") 3], Nothing), ([], Nothing), ([], Nothing)]
        agrees ["a", "b"] units
        Right (rels, _) <- pure (runConv (jumpsToRelevance (T.pack . show) units))
        take 2 rels @?= [Nothing, Just (ENot (ERef "a"))]
    , testCase "a chain whose next screen does not continue it" $ do
        -- b0 "a -> b1, b -> b2, else b3"; b1 "else b3".
        let units = [([Rule (ERef "a") 1, Rule (ERef "b") 2], Just 3), ([], Just 3), ([], Nothing), ([], Nothing)]
        agrees ["a", "b"] units
    , testCase "a chain entered from outside" $ do
        -- Screen 0 jumps into the middle of screen 1's chain.
        let units = [([Rule (ERef "c") 2], Nothing), ([Rule (ERef "a") 2, Rule (ERef "b") 3], Just 4), ([Rule (ERef "b") 3], Just 4), ([], Nothing), ([], Nothing)]
        agrees ["a", "b", "c"] units
    , testCase "generated layouts agree with a walk through the screens" $
        forM_ layouts (agrees ["a", "b", "c"])
    , testCase "the writer's chains read back as written" $ do
        let cases =
              [ [Nothing, Just (ERef "a"), Just (ERef "b"), Nothing, Just (ERef "c"), Nothing]
              , [Nothing, Just (ERef "a"), Just (ERef "a"), Just (ERef "b")]
              , [Nothing, Nothing, Just (EBin And (ERef "a") (ERef "b"))]
              ]
        forM_ cases $ \rels -> do
          fmap fst (runConv (jumpsToRelevance (T.pack . show) (relevanceToJumps rels))) @?= Right rels
          agrees ["a", "b", "c"] (relevanceToJumps rels)
    , testCase "a jump over a whole chain keeps the chain's conditions" $ do
        let units = [([Rule (ERef "c") 4], Nothing), ([Rule (ERef "a") 2, Rule (ERef "b") 3], Just 4), ([Rule (ERef "b") 3], Just 4), ([], Nothing), ([], Nothing)]
        agrees ["a", "b", "c"] units
        fmap fst (runConv (jumpsToRelevance (T.pack . show) units))
          @?= Right [Nothing, Just (ENot (ERef "c")), Just (EBin And (ERef "a") (ENot (ERef "c"))), Just (EBin And (ERef "b") (ENot (ERef "c"))), Nothing]
    , testCase "an unconditional source leaves the old conditions unchanged" $ do
        let units = [([Rule (ERef "a") 2, Rule (ERef "b") 3], Nothing), ([], Nothing), ([], Nothing), ([], Nothing)]
        fmap fst (runConv (jumpsToRelevance (T.pack . show) units))
          @?= Right
            [ Nothing
            , Just (ENot (EBin Or (ERef "a") (EBin And (ERef "b") (ENot (ERef "a")))))
            , Just (ENot (EBin And (ERef "b") (ENot (ERef "a"))))
            , Nothing
            ]
    , testCase "runaway jump logic warns instead of failing or exhausting memory" $ do
        -- Every screen can jump to the end from many rules.
        let n = 700
            wide = [([Rule (ERef "a") n | _ <- [1 :: Int .. 2]], Nothing) | _ <- [1 .. n]]
        r <- timeout 30000000 $ case runConv (jumpsToRelevance (T.pack . show) wide) of
          Left e -> assertFailure (show e)
          Right (rels, ws) -> do
            assertBool "reported" (warned "no show-if conditions derived" ws)
            length (filter isJust' rels) @?= 0
        maybe (assertFailure "did not finish") pure r
    , testCase "a long chain of branching jumps converts exactly and fast" $ do
        -- 120 screens, each jumping two or three screens ahead on one of
        -- three answers: built term by term, every condition nests the
        -- ones before it.
        let n = 120
            v i = ["a", "b", "c"] !! (i `mod` 3)
            deep = [([Rule (ERef (v i)) (i + 2), Rule (ERef (v (i + 1))) (i + 3)], Nothing) | i <- [0 .. n - 4]] ++ replicate 3 ([], Nothing)
        r <- timeout 20000000 $ do
          agrees ["a", "b", "c"] deep
          case runConv (jumpsToRelevance (T.pack . show) deep) of
            Left e -> assertFailure (show e)
            Right (rels, ws) -> do
              ws @?= []
              _ <- evaluate (length (show rels))
              pure ()
        maybe (assertFailure "did not finish") pure r
    , testCase "formbricks: 120 questions with branching jumps read, convert and write" $ do
        -- Every question jumps two or three ahead on its own answer, so
        -- whether a late question is reached depends on every answer
        -- before it: no show-if condition of sane size is exact. The read
        -- keeps those questions unconditional, says so, and keeps the
        -- jumps for Formbricks.
        let n = 120 :: Int
            qid i = "q" <> T.pack (show i)
            cond i val = "{\"id\":\"g\",\"connector\":\"and\",\"conditions\":[{\"id\":\"c\",\"leftOperand\":{\"type\":\"question\",\"value\":\"" <> qid i <> "\"},\"operator\":\"equals\",\"rightOperand\":{\"type\":\"static\",\"value\":\"" <> val <> "\"}}]}"
            jump i val t = "{\"id\":\"l" <> val <> "\",\"conditions\":" <> cond i val <> ",\"actions\":[{\"id\":\"x\",\"objective\":\"jumpToQuestion\",\"target\":\"" <> qid t <> "\"}]}"
            logic i
              | i + 3 <= n = ",\"logic\":[" <> jump i "a" (i + 2) <> "," <> jump i "b" (i + 3) <> "]"
              | otherwise = ""
            q i = "{\"id\":\"" <> qid i <> "\",\"type\":\"openText\",\"headline\":{\"default\":\"Question " <> T.pack (show i) <> "\"},\"required\":false" <> logic i <> "}"
            doc = "{\"name\":\"long\",\"questions\":[" <> T.intercalate "," (map q [1 .. n]) <> "]}"
        r <- timeout 30000000 $ do
          (s, ws) <- readAs "formbricks" (BL.fromStrict (TE.encodeUtf8 doc))
          assertBool "approximation reported" (warned "show-if condition from the jumps needs more than" ws)
          assertBool "jumps kept" (warned "jump logic kept as written for formbricks" ws)
          length (questions s) @?= n
          -- Early questions still get their exact condition.
          assertBool "q2 keeps its condition" (isJust' (relOf s "q2"))
          relOf s "q100" @?= Nothing
          (out, _) <- writeAs "formbricks" s
          -- The jumps are written back as they were read.
          let logicOf bs = [(keyText' "id" q, KM.lookup "logic" q, KM.lookup "logicFallback" q) | Just (Object o) <- [decode' bs], Just (Array qs0) <- [KM.lookup "questions" o], Object q <- toList' qs0]
          logicOf out @?= logicOf (BL.fromStrict (TE.encodeUtf8 doc))
          (back, _) <- readAs "formbricks" out
          (again, _) <- writeAs "formbricks" back
          again @?= out
          forM_ ["xlsform", "surveyjs", "tally"] $ \f -> writeAs f s >>= evaluate . BL.length . fst
        maybe (assertFailure "did not finish") pure r
    , testCase "formbricks: jump from a skipped question (v1 questions)" $ do
        let q i extra = "{\"id\":\"" <> i <> "\",\"type\":\"openText\",\"headline\":{\"default\":\"" <> i <> "\"},\"required\":false" <> extra <> "}"
            cond l op r = "{\"id\":\"g\",\"connector\":\"and\",\"conditions\":[{\"id\":\"c\",\"leftOperand\":{\"type\":\"question\",\"value\":\"" <> l <> "\"},\"operator\":\"" <> op <> "\"" <> r <> "}]}"
            logic c t = ",\"logic\":[{\"id\":\"l\",\"conditions\":" <> c <> ",\"actions\":[{\"id\":\"x\",\"objective\":\"jumpToQuestion\",\"target\":\"" <> t <> "\"}]}]"
            doc =
              "{\"name\":\"t\",\"questions\":["
                <> q "q1" (logic (cond "q1" "equals" ",\"rightOperand\":{\"type\":\"static\",\"value\":\"a\"}") "q3")
                <> ","
                <> q "q2" (logic (cond "q2" "isSkipped" "") "q4")
                <> ","
                <> q "q3" ""
                <> ","
                <> q "q4" ""
                <> "]}"
        (s, _) <- readAs "formbricks" (BLC.pack doc)
        let env :: Bool -> Bool -> M.Map T.Text Bool
            env a answered = M.fromList [("q1", a), ("q2ans", answered)]
            holds e a answered = evalQ (env a answered) e
            evalQ :: M.Map T.Text Bool -> Expr -> Bool
            evalQ m = \case
              ELit (LBool b) -> b
              EBin Eq (ERef "q1") (ELit (LStr "a")) -> M.findWithDefault False "q1" m
              EBin Neq (ERef "q1") (ELit (LStr "a")) -> not (M.findWithDefault False "q1" m)
              EAnswered "q2" -> M.findWithDefault False "q2ans" m
              ENot x -> not (evalQ m x)
              EBin And x y -> evalQ m x && evalQ m y
              EBin Or x y -> evalQ m x || evalQ m y
              e -> error (show e)
        -- With the answer a, q2 is skipped; its "is skipped" jump must not
        -- hide q3.
        fmap (\e -> holds e True False) (relOf s "q3") @?= Just True
        -- Without a, q2 is shown and skipping it hides q3.
        fmap (\e -> holds e False False) (relOf s "q3") @?= Just False
        fmap (\e -> holds e False True) (relOf s "q3") @?= Just True
    ]

------------------------------------------------------------------------------
-- mrScript and Blaise routing

routingTests :: TestTree
routingTests =
  testGroup
    "routing"
    [ testCase "mrscript: ElseIf and Else exclude every earlier branch" $ do
        (s, _) <- readAs "mrscript" (mrs ["If a = 1 Then", "q1.Ask()", "ElseIf b = 1 Then", "q2.Ask()", "ElseIf b = 2 Then", "q3.Ask()", "Else", "q4.Ask()", "End If"])
        let a1 = EBin Eq (ERef "a") (ELit (LNum 1))
            na1 = EBin Neq (ERef "a") (ELit (LNum 1))
            b op n = EBin op (ERef "b") (ELit (LNum n))
        relOf s "q1" @?= Just a1
        relOf s "q2" @?= Just (EBin And na1 (b Eq 1))
        relOf s "q3" @?= Just (EBin And (EBin And na1 (b Neq 1)) (b Eq 2))
        relOf s "q4" @?= Just (EBin And (EBin And na1 (b Neq 1)) (b Neq 2))
    , testCase "mrscript: a question asked on two paths is shown on either" $ do
        (s, ws) <- readAs "mrscript" (mrs ["If a = 1 Then q1.Ask()", "If b = 1 Then q1.Ask()", "q2.Ask()", "If a = 1 Then q2.Ask()"])
        relOf s "q1" @?= Just (EBin Or (EBin Eq (ERef "a") (ELit (LNum 1))) (EBin Eq (ERef "b") (ELit (LNum 1))))
        relOf s "q2" @?= Nothing
        assertBool "repeat reported" (warned "asked more than once" ws)
    , testCase "blaise: a type defined through itself ends in a warning" $ do
        (s, ws) <- readAs "blaise" "DATAMODEL Loop\nTYPE\n  A = B\n  B = A\nFIELDS\n  q \"Q?\" : A\nRULES\n  q\nENDMODEL\n"
        map qId (questions s) @?= ["q"]
        assertBool "cycle reported" (warned "defined in terms of itself" ws)
    , testCase "blaise: a field asked in both branches is always asked" $ do
        (s, ws) <-
          readAs "blaise" "DATAMODEL M\nFIELDS\n  flag \"F?\" : (Yes, No)\n  r \"R?\" : STRING[10]\n  t \"T?\" : STRING[10]\n  x : INTEGER\nRULES\n  flag\n  IF flag = Yes THEN\n    r\n    x := 1\n  ELSE\n    r\n    x := 2\n  ENDIF\n  IF flag = No THEN\n    t\n  ENDIF\n  t\nENDMODEL\n"
        let yes = EBin Eq (ERef "flag") (ELit (LStr "Yes"))
        relOf s "r" @?= Just (EBin Or yes (EBin Neq (ERef "flag") (ELit (LStr "Yes"))))
        relOf s "t" @?= Nothing
        assertBool "repeated ask reported" (warned "asked more than once" ws)
        assertBool "repeated assignment reported" (warned "assigned more than once" ws)
    ]
  where
    mrs routing =
      BLC.pack . unlines $
        ["Metadata(en-US, Question, Label)", "    a \"A?\" long [0 .. 9];", "    b \"B?\" long [0 .. 9];"]
          ++ ["    q" <> show n <> " \"Q" <> show n <> "\" text;" | n <- [1 :: Int .. 4]]
          ++ ["End Metadata", "", "Routing(Web)", "    a.Ask()", "    b.Ask()"]
          ++ map ("    " <>) routing
          ++ ["End Routing"]

------------------------------------------------------------------------------
-- LimeSurvey

limeSurveyTests :: TestTree
limeSurveyTests =
  testGroup
    "limesurvey"
    [ testCase "a quoted answer that looks like a field name is left alone" $ do
        lss <- miniLss
        (s, _) <- readAs "limesurvey" (patch [(ownRelevance, cell "relevance" "comment == \"1X2X1\" and 123456X1X1 > 3")] lss)
        relOf s "T" @?= Just (EBin And (EBin Eq (ERef "comment") (ELit (LStr "1X2X1"))) (EBin Gt (ERef "Age") (ELit (LNum 3))))
    , testCase "legacy conditions on one field are or-ed wherever their rows are" $ do
        lss <- miniLss
        let cond n f v = [("cid", T.pack (show (n :: Int))), ("qid", "4"), ("cqid", "0"), ("cfieldname", f), ("method", "=="), ("value", v), ("scenario", "1")]
            conds = table "conditions" [cond 1 "123456X1X2" "Red", cond 2 "123456X1X1" "5", cond 3 "123456X1X2" "Blue"]
        (s, _) <- readAs "limesurvey" (patch [(ownRelevance, cell "relevance" "1"), ("</document>", conds <> "</document>")] lss)
        let col v = EBin Eq (ERef "Col") (ELit (LStr v))
        relOf s "T" @?= Just (EBin And (EBin Or (col "Red") (col "Blue")) (EBin Eq (ERef "Age") (ELit (LStr "5"))))
    , testCase "answers keep their file order" $ do
        lss <- miniLss
        (s, _) <- readAs "limesurvey" (patch [] lss)
        [map choiceValue cs | q <- questions s, qId q == "Col", QChoice _ _ cs <- [qType q]] @?= [["Red", "Blue", "Green"]]
    , testCase "a huge rating scale is not written out point by point" $ do
        let q = question "big" "Big" (QScale 0 2000000000 Nothing Nothing)
            form = Form emptyMeta [ItemQuestion q]
        r <- timeout 30000000 $ do
          (out, ws) <- writeAs "limesurvey" form
          _ <- evaluate (BL.length out)
          (tsv, _) <- writeAs "limesurvey-tsv" form
          _ <- evaluate (BL.length tsv)
          pure (out, ws)
        case r of
          Nothing -> assertFailure "writing did not finish"
          Just (out, ws) -> do
            assertBool "reported" (warned "written as a whole-number question" ws)
            assertBool "small" (BL.length out < 20000)
            (back, _) <- readAs "limesurvey" out
            map qType (questions back) @?= [QNumber Integer]
            map qValidation (questions back) @?= [Just (Validation (Just 0) (Just 2000000000) Nothing Nothing)]
            (again, ws2) <- writeAs "limesurvey" back
            again @?= out
            ws2 @?= []
    , testCase "an ordinary rating scale is still a list" $ do
        (out, _) <- writeAs "limesurvey" (Form emptyMeta [ItemQuestion (question "nps" "NPS" (QScale 0 10 Nothing Nothing))])
        (back, _) <- readAs "limesurvey" out
        [length cs | q <- questions back, QChoice _ _ cs <- [qType q]] @?= [11]
    ]
  where
    miniLss = do
      (s, _) <- readAs "questionpunk" "1. Age? {#Age integer}\n\n2. Colour? {#Col}\n   ( ) Red\n   ( ) Blue\n   ( ) Green\n\n3. Comment {#comment}\n\n4. Target {#T}\n   if comment = \"x\"\n"
      utf8 . fst <$> writeAs "limesurvey" s
    ownRelevance = cell "relevance" "comment.NAOK == \"x\""
    cell f v = "<" <> f <> "><![CDATA[" <> v <> "]]></" <> f <> ">"
    patch subs t =
      BL.fromStrict . TE.encodeUtf8 $
        foldl (\acc (a, b) -> if a `T.isInfixOf` acc then T.replace a b acc else error ("patch target missing: " <> T.unpack a)) t subs
    table name rows =
      T.unlines $
        [" <" <> name <> ">", "  <fields>"]
          ++ ["   <fieldname>" <> f <> "</fieldname>" | (f, _) <- concat (take 1 rows)]
          ++ ["  </fields>", "  <rows>"]
          ++ concat [["   <row>"] ++ ["    " <> cell f v | (f, v) <- r] ++ ["   </row>"] | r <- rows]
          ++ ["  </rows>", " </" <> name <> ">"]

------------------------------------------------------------------------------
-- Expression dialects: strings, choice values, negation

dialects :: [Dialect]
dialects = [xlsform, redcap, surveyjs, limesurvey, rlang, decipher, mrscript, blaise, xpath ("/data/" <>)]

-- | Texts made of the characters that need care inside a string.
genTricky :: Gen T.Text
genTricky = T.pack <$> listOf1 (elements "ab' \"\\nt\n\t")

genId :: Gen T.Text
genId = elements ["q1", "q2", "age", "colour"]

-- | Printing either fails or reads back as the same expression.
printsBack :: Dialect -> Expr -> Property
printsBack d e = case printExpr d e of
  Left _ -> property True
  Right src -> counterexample (T.unpack src) (parseExpr d src === Right e)

-- | Conditions with negations in every position, no double negation.
genNeg :: Gen Expr
genNeg = sized go
  where
    cmp = EBin <$> elements [Eq, Neq, Lt, Ge] <*> (ERef <$> genId) <*> (ELit . LNum . fromIntegral <$> chooseInt (0, 9))
    leaf = oneof [cmp, ESelected <$> genId <*> elements ["a", "b1"], EAnswered <$> genId]
    notOf x = case x of
      ENot _ -> x
      _ -> ENot x
    go n
      | n <= 1 = oneof [leaf, notOf <$> leaf]
      | otherwise =
          oneof
            [ leaf
            , notOf <$> go (n `div` 2)
            , EBin <$> elements [And, Or] <*> go (n `div` 2) <*> go (n `div` 2)
            , (\a k -> EBin Eq (ENot (ERef a)) (ELit (LNum (fromIntegral k)))) <$> genId <*> chooseInt (0, 1)
            ]

exprTests :: TestTree
exprTests =
  testGroup
    "expression dialects"
    [ testGroup
        "a string literal reads back as written"
        [ testProperty (T.unpack (dName d)) $ forAll ((,) <$> genId <*> genTricky) $ \(q, v) ->
            printsBack d (EBin Eq (ERef q) (ELit (LStr v)))
        | d <- dialects
        ]
    , testGroup
        "a choice value reads back as written"
        [ testProperty (T.unpack (dName d)) $ forAll ((,) <$> genId <*> genTricky) $ \(q, v) ->
            printsBack d (ESelected q v) .&&. printsBack d (ENot (ESelected q v))
        | d <- dialects
        ]
    , testGroup
        "negation reads back as written"
        [ testProperty (T.unpack (dName d)) $ forAll genNeg $ \e -> case dNot d of
            Just _ -> printsBack d e
            -- With no negation operator it is pushed inward; the result
            -- must still be stable.
            Nothing -> case printExpr d e of
              Left _ -> property True
              Right src -> counterexample (T.unpack src) ((parseExpr d src >>= either (Left . T.unpack) Right . printExpr d) === Right src)
        | d <- dialects
        ]
    , testCase "an apostrophe in a choice value" $ do
        printExpr surveyjs (ESelected "q1" "Don't know") @?= Right "{q1} contains 'Don\\'t know'"
        printExpr xlsform (ESelected "q1" "Don't know") @?= Right "selected(${q1}, \"Don't know\")"
        printExpr xlsform (ESelected "q1" "both ' and \"") @?= Left "a text containing both kinds of quote"
    , testCase "a backslash is not an escape where the dialect has none" $ do
        parseExpr xlsform "selected(${q}, 'C:\\temp')" @?= Right (ESelected "q" "C:\\temp")
        parseExpr surveyjs "{q} = 'C:\\temp'" @?= Right (EBin Eq (ERef "q") (ELit (LStr "C:\\temp")))
    , testCase "python, R and mrScript negation binds looser than a comparison" $ do
        let a1 = EBin Eq (ERef "a") (ELit (LNum 1))
            b2 = EBin Eq (ERef "b") (ELit (LNum 2))
        parseExpr decipher "not a.val == 1 and b.val == 2" @?= Right (EBin And (ENot a1) b2)
        parseExpr rlang "!input$a == 1 & input$b == 2" @?= Right (EBin And (ENot a1) b2)
        parseExpr mrscript "Not a = 1 And b = 2" @?= Right (EBin And (ENot a1) b2)
        printExpr decipher (EBin Eq (ENot (ERef "a")) (ELit (LNum 1))) @?= Right "(not a.val) == 1"
        -- Elsewhere a prefix negation still binds tightest.
        parseExpr surveyjs "!{a} = 1" @?= Right (EBin Eq (ENot (ERef "a")) (ELit (LNum 1)))
    , testCase "redcap negates a group of membership and answer tests" $
        printExpr redcap (ENot (EBin And (ESelected "q" "a") (EAnswered "r"))) @?= Right "[q(a)] = '0' or [r] = ''"
    , testCase "a choice code the dialect would misread is refused" $ do
        assertBool "limesurvey underscore" (isLeft (printExpr limesurvey (ESelected "q" "a_b")))
        assertBool "decipher any" (isLeft (printExpr decipher (ESelected "q" "any")))
    , testCase "a translated scale label counts as a translation" $ do
        let lowFr = I18n "Bad" (M.fromList [("fr", "Mauvais")])
            form = Form emptyMeta [ItemQuestion (question "r" "Rate" (QScale 1 5 (Just lowFr) Nothing))]
        (_, ws) <- writeAs "decipher" form
        assertBool "decipher reports it" (warned "translations dropped (fr)" ws)
    ]
  where
    isLeft = either (const True) (const False)

------------------------------------------------------------------------------
-- Typeform, Jotform, Tally, Decipher, Formbricks, LimeSurvey .lss

formatTests :: TestTree
formatTests =
  testGroup
    "formats"
    [ testCase "typeform: jumps after a raw jump keep their order" $ do
        let doc =
              "{\"title\":\"t\",\"fields\":["
                <> T.intercalate "," ["{\"ref\":\"" <> r <> "\",\"title\":\"" <> r <> "\",\"type\":\"short_text\"}" | r <- ["a", "b", "c", "d"]]
                <> "],\"logic\":[{\"type\":\"field\",\"ref\":\"b\",\"actions\":["
                <> "{\"action\":\"jump\",\"details\":{\"to\":{\"type\":\"field\",\"value\":\"a\"}},\"condition\":{\"op\":\"always\",\"vars\":[]}},"
                <> "{\"action\":\"jump\",\"details\":{\"to\":{\"type\":\"field\",\"value\":\"d\"}},\"condition\":{\"op\":\"is\",\"vars\":[{\"type\":\"field\",\"value\":\"b\"},{\"type\":\"constant\",\"value\":\"x\"}]}}"
                <> "]}]}"
        (s, ws) <- readAs "typeform" (BL.fromStrict (TE.encodeUtf8 doc))
        -- The backward jump always fires first, so c is never skipped.
        relOf s "c" @?= Nothing
        assertBool "reported" (warned "jump after an unconverted jump" ws)
        (out, _) <- writeAs "typeform" s
        let o = utf8 out
            pos needle = T.length (fst (T.breakOn needle o))
        assertBool "both jumps written" (pos "\"value\": \"d\"" < T.length o)
        assertBool "the backward jump stays first" (pos "\"value\": \"a\"" < pos "\"value\": \"d\"")
    , testCase "typeform: a statement's description joined to its text is reported" $ do
        (_, ws) <- readAs "typeform" "{\"title\":\"t\",\"fields\":[{\"ref\":\"s\",\"title\":\"Hello\",\"type\":\"statement\",\"properties\":{\"description\":\"More\"}}]}"
        assertBool "reported" (warned "statement description joined" ws)
    , testCase "jotform and tally: a renamed question does not take another's references" $ do
        -- The blank question would be named q1; the real q1 keeps its name
        -- and the condition on q1 still points at it.
        let blank = (question "" "blank" (QText ShortText))
            real = question "q1" "real" (QText ShortText)
            t = (question "t" "T" (QText ShortText)) {qRelevant = Just (EBin Eq (ERef "q1") (ELit (LStr "yes")))}
            form = Form emptyMeta (map ItemQuestion [blank, real, t])
        forM_ ["jotform", "tally"] $ \f -> do
          (out, _) <- writeAs f form
          (back, _) <- readAs f out
          let ids = map qId (questions back)
          assertBool (f <> " names unique: " <> show ids) (length ids == length (M.fromList (zip ids ids)))
          case [q | q <- questions back, qLabel q == plain "real"] of
            [r] -> do
              qId r @?= "q1"
              fmap refsOf (relOf back "t") @?= Just ["q1"]
            other -> assertFailure (f <> ": " <> show (map qId other))
        -- A first question named q2 and a blank second one.
        let form2 = Form emptyMeta (map ItemQuestion [question "q2" "first" (QText ShortText), question "" "second" (QText ShortText)])
        forM_ ["jotform", "tally"] $ \f -> do
          (out, _) <- writeAs f form2
          (back, _) <- readAs f out
          let ids = map qId (questions back)
          assertBool (f <> " names unique: " <> show ids) (length ids == length (M.fromList (zip ids ids)))
    , testCase "decipher: question children it does not read are reported" $ do
        (_, ws) <-
          readAs "decipher" "<survey alt=\"t\"><number label=\"age\" size=\"3\"><title>Age?</title><validate>if this.ival > 120: error(\"x\")</validate></number><text label=\"n\"><title>N</title><row label=\"r1\">A</row></text></survey>"
        assertBool "validate" (warned "<validate> inside the question dropped" ws)
        assertBool "row" (warned "<row> inside the question dropped" ws)
    , testCase "formbricks: bounds joined by or are not a range" $ do
        let doc =
              "{\"name\":\"t\",\"questions\":[{\"id\":\"n\",\"type\":\"openText\",\"inputType\":\"number\",\"headline\":{\"default\":\"N\"},\"required\":false,"
                <> "\"validation\":{\"rules\":[{\"id\":\"a\",\"type\":\"minValue\",\"params\":{\"min\":10}},{\"id\":\"b\",\"type\":\"maxValue\",\"params\":{\"max\":0}}],\"logic\":\"or\"}}]}"
        (s, ws) <- readAs "formbricks" doc
        map qValidation (questions s) @?= [Nothing]
        assertBool "reported" (warned "validation rules kept only for formbricks" ws)
        (out, _) <- writeAs "formbricks" s
        assertBool "kept with its connector" (has "\"logic\": \"or\"" out)
    , testCase "limesurvey: a carriage return survives an XML parser" $ do
        let form = Form emptyMeta [ItemQuestion (question "q1" "one\r\ntwo\rend" (QText ShortText))]
        (out, _) <- writeAs "limesurvey" form
        assertBool "no raw carriage return" (not (BLC.elem '\r' out))
        (back, _) <- readAs "limesurvey" out
        map qLabel (questions back) @?= [plain "one\r\ntwo\rend"]
    ]
  where
    refsOf = refs
