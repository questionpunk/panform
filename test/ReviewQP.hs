{-# LANGUAGE OverloadedStrings #-}

-- | QuestionPunk Markdown, its logic language and the app JSON: cases found
-- in the pre-release review.
module ReviewQP (tests) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Expr.QP (parseQP, printQP)
import Panform.Format.QuestionPunk (readQPMarkdown, writeQPMarkdown)
import Panform.Format.QuestionPunkJson (readQPJson, writeQPJson)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "questionpunk review"
    [ testGroup "logic prints what it reads back" (map printsAs logicCases)
    , testCase "a - 1 is a subtraction and -1 a number" $ do
        parseQP "q = $a - 1" @?= Right (EBin Eq (ERef "q") (EBin Sub (ERef "a") (num 1)))
        parseQP "q = -1" @?= Right (EBin Eq (ERef "q") (num (-1)))
    , testGroup
        "a section ends where the tree says"
        [ testCase "an item after a section stays outside it" $ sameTree [sec "s1" [q "a"], ItemQuestion (question "b" "B" (QText ShortText))]
        , testCase "a note after a section stays outside it" $ sameTree [sec "s1" [q "a"], ItemNote "n" (plain "Thanks") Nothing]
        , testCase "an item after a nested section stays in the outer one" $
            sameTree [sec "s1" [sub "s2" [q "a"], q "b"], q "c"]
        , testCase "sections in a row need no end line" $ do
            out <- written [sec "s1" [q "a"], sec "s2" [q "b"]]
            assertBool "no bare heading marks" (not (any (T.all (== '#')) (filter (not . T.null) (T.lines out))))
        , testCase "the end line is this level's heading marks alone" $ do
            out <- written [sec "s1" [q "a"], q "b"]
            assertBool (T.unpack out) ("\n\n#\n\n" `T.isInfixOf` out)
        ]
    , testCase "a byte-order mark does not hide the front matter" $ do
        (form, _) <- ok (readQPMarkdown (BL.fromStrict (TE.encodeUtf8 "\xFEFF---\ntitle: Hello\n---\n\n1. Name? {#name}\n")))
        fmap i18nDefault (metaTitle (formMeta form)) @?= Just "Hello"
        length (formItems form) @?= 1
    , testCase "an empty ranking or matrix is reported" $ do
        (_, ws1) <- ok (writeQPMarkdown (Form emptyMeta [ItemQuestion (question "r" "Rank" (QRanking []))]))
        assertBool "ranking" (any (("ranking" `T.isInfixOf`) . warnWhat) ws1)
        (_, ws2) <- ok (writeQPMarkdown (Form emptyMeta [ItemQuestion (question "m" "Grid" (QMatrix Single [] [choice "1" "Yes"]))]))
        assertBool "matrix" (any (("matrix" `T.isInfixOf`) . warnWhat) ws2)
    , testCase "questions that share a name keep their own type in the app JSON" $ do
        let items =
              [ ItemQuestion (question "x" "Say more" (QText LongText))
              , ItemQuestion (question "x" "Pick one" (QChoice Single Radio [choice "a" "A", choice "b" "B"]))
              ]
        (bytes, _) <- ok (writeQPJson (Form emptyMeta items))
        map (field "answer_type") (surveyItems bytes) @?= [Just "long_text", Just "multiple_choice"]
    , testCase "a condition naming a shared name tests the first question with it" $ do
        let items =
              [ ItemQuestion (question "freq" "How often?" (QChoice Single Radio [choice "Daily" "Daily", choice "Weekly" "Weekly"]))
              , ItemQuestion (question "freq" "Why?" (QText LongText))
              , ItemQuestion ((question "more" "Tell us more" (QText LongText)) {qRelevant = Just (EBin Eq (ERef "freq") (ELit (LStr "Daily")))})
              ]
        (bytes, _) <- ok (writeQPJson (Form emptyMeta items))
        case decode bytes of
          Just (Object o) | Just (Array ls) <- KM.lookup "survey_display_logic" o, [Object c] <- toList ls, Just (Array gs) <- KM.lookup "groups" c, [Object g] <- toList gs ->
            KM.lookup "source_question" g @?= Just (Number 1)
          other -> assertFailure ("no single display condition: " <> show other)
    , testCase "a line of heading marks that ends nothing is reported" $ do
        (form, ws) <- ok (readQPMarkdown "# Product {#p}\n\n1. One? {#one}\n\n##\n\n2. Two? {#two}\n")
        assertBool (show (map warnWhat ws)) (any (("end no open section" `T.isInfixOf`) . warnWhat) ws)
        length [() | ItemGroup g <- formItems form, length (groupItems g) == 2] @?= 1
    , testCase "a line of heading marks that ends a section is not reported" $ do
        (_, ws) <- ok (readQPMarkdown "# Product {#p}\n\n1. One? {#one}\n\n#\n\n2. Two? {#two}\n")
        assertBool (show (map warnWhat ws)) (not (any (("end no open section" `T.isInfixOf`) . warnWhat) ws))
    , testCase "a position name never repeats a name already used" $ do
        let payload =
              object
                [ "survey_items"
                    .= [ object ["answer_type" .= ("short_text" :: T.Text), "text" .= ("First" :: T.Text), "internal_name" .= ("q2" :: T.Text)]
                       , object ["answer_type" .= ("short_text" :: T.Text), "text" .= ("Second" :: T.Text)]
                       ]
                ]
        (form, _) <- ok (readQPJson (encodeValue payload))
        let ids = [qId x | ItemQuestion x <- formItems form]
        length ids @?= 2
        assertBool (show ids) (length ids == length (dedupe ids))
    , testCase "a branch back to an earlier question is reported" $ do
        let item t = object ["answer_type" .= ("multiple_choice" :: T.Text), "text" .= (t :: T.Text), "itemDetails" .= object ["answer_options" .= (["A", "B"] :: [T.Text])]]
            payload =
              object
                [ "survey_items" .= [item "One", item "Two", item "Three"]
                , "survey_logic"
                    .= [ object
                           [ "source_question" .= (2 :: Int)
                           , "groups" .= [object ["condition" .= ("or" :: T.Text), "destination_question" .= (1 :: Int), "rules" .= [object ["type" .= ("is_selected_option" :: T.Text), "option_index" .= (0 :: Int)]]]]
                           , "fallback_question" .= (3 :: Int)
                           ]
                       ]
                ]
        (_, ws) <- ok (readQPJson (encodeValue payload))
        assertBool (show (map warnWhat ws)) (any (("earlier question" `T.isInfixOf`) . warnWhat) ws)
    , testCase "a scale with extreme ends does not enumerate" $ do
        let wide = question "s" "How many?" (QScale minBound maxBound Nothing Nothing)
        (bytes, _) <- ok (writeQPJson (Form emptyMeta [ItemQuestion wide]))
        map (field "answer_type") (surveyItems bytes) @?= [Just "numeric"]
    ]
  where
    num = ELit . LNum
    q i = ItemQuestion (question i (T.toUpper i) (QText ShortText))
    sec i kids = ItemGroup (Group i Page Nothing Nothing kids mempty)
    sub i kids = ItemGroup (Group i Section Nothing Nothing kids mempty)
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    encodeValue :: Value -> BL.ByteString
    encodeValue = encode

-- | A condition and the text it must print as; reading that text gives the
-- condition back.
logicCases :: [(Expr, T.Text)]
logicCases =
  [ (EBin Eq (ERef "q") (EBin Mul (EBin Add (ERef "a") n1) n2), "q = ($a + 1) * 2")
  , (EBin Eq (ERef "q") (EBin Add (ERef "a") (EBin Mul n1 n2)), "q = $a + 1 * 2")
  , (EBin Eq (ERef "q") (EBin Sub (ERef "a") (EBin Sub (ERef "b") n1)), "q = $a - ($b - 1)")
  , (EBin Eq (ERef "q") (EBin Sub (EBin Sub (ERef "a") (ERef "b")) n1), "q = $a - $b - 1")
  , (EBin Eq (ERef "q") (EBin Div (EBin Add (ERef "a") (ERef "b")) n2), "q = ($a + $b) / 2")
  , (EBin Le (ERef ".") (ECall "today" []), ". <= today()")
  , (EBin Mul (EBin Add (ERef "a") (ERef "b")) (ERef "c"), "(a + b) * c")
  , (EBin And (ELit (LStr "abc")) (EAnswered "x"), "\"abc\" and x is answered")
  , (EBin Add (EBin Eq (ERef "a") n1) n2, "(a = 1) + 2")
  , (EBin Add (ENot (ERef "x")) n1, "(not x) + 1")
  , (EBin Mul (EAnswered "x") n2, "(x is answered) * 2")
  , (EBin Add (ESelected "x" "y") n1, "(x has y) + 1")
  , (EBin Add (EBin Eq (ECall "f" []) n1) n2, "(f() = 1) + 2")
  , (EBin And (ENot (EBin Eq (ERef "a") n1)) (EBin Eq (ERef "b") n2), "not a = 1 and b = 2")
  ]
  where
    n1 = ELit (LNum 1)
    n2 = ELit (LNum 2)

printsAs :: (Expr, T.Text) -> TestTree
printsAs (e, text) = testCase (T.unpack text) $ do
  printQP e @?= Right text
  parseQP text @?= Right e

ok :: Conv a -> IO (a, [Warning])
ok c = either (assertFailure . show) pure (runConv c)

written :: [Item] -> IO T.Text
written items = TE.decodeUtf8 . BL.toStrict . fst <$> ok (writeQPMarkdown (Form emptyMeta items))

-- | Writing and reading back gives the same tree, and the same bytes again.
sameTree :: [Item] -> Assertion
sameTree items = do
  (bytes, _) <- ok (writeQPMarkdown (Form emptyMeta items))
  (form, _) <- ok (readQPMarkdown bytes)
  formItems form @?= items
  (again, _) <- ok (writeQPMarkdown form)
  again @?= bytes

surveyItems :: BL.ByteString -> [Value]
surveyItems bytes = case decode bytes of
  Just (Object o) | Just (Array xs) <- KM.lookup "survey_items" o -> toList xs
  _ -> []

field :: T.Text -> Value -> Maybe T.Text
field k = \case
  Object o | Just (String s) <- KM.lookup (K.fromText k) o -> Just s
  _ -> Nothing
