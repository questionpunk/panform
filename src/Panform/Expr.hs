{-# LANGUAGE OverloadedStrings #-}

-- | Logic-expression dialects. Every format spells "show Q2 when Q1 is yes"
-- differently; each 'Dialect' here is one spelling with a parser into and a
-- printer out of the neutral 'Expr'.
--
-- > ${q1} = 'yes' and selected(${q2}, 'b')      -- XLSForm / ODK
-- > [q1] = 'yes' and [q2(b)] = '1'              -- REDCap
-- > {q1} = 'yes' and {q2} contains 'b'          -- SurveyJS
-- > q1 == "yes" && q2_b == "Y"                  -- LimeSurvey ExpressionScript
-- > input$q1 == "yes" & "b" %in% input$q2       -- R (surveydown, formr)
-- > q1.yes and q2.b                              -- Forsta/Decipher (Python)
-- > q1.ContainsAny({yes}) And q2.ContainsAny({b}) -- Dimensions mrScript
-- > (q1 = yes) AND (b IN q2)                     -- Blaise
module Panform.Expr
  ( Dialect (..)
  , Strings (..)
  , xlsform
  , redcap
  , surveyjs
  , limesurvey
  , rlang
  , decipher
  , mrscript
  , blaise
  , xpath
  , parseExpr
  , printExpr
  , parseOrRaw
  , printLogic
  , pushNot
  , refs
  , substSelf
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (showFFloat)
import Panform.AST hiding (choice)
import Panform.Conv (Conv, warn)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Dialect = Dialect
  { dName :: Text
  , dAtoms :: [Parser Expr]
  -- ^ Dialect-specific atoms (references, membership tests), tried before
  -- literals.
  , dPostfix :: [Parser (Expr -> Expr)]
  , dOps :: [(BinOp, [Text])]
  -- ^ Accepted spellings; the first is the one printed.
  , dNot :: Maybe [Text]
  -- ^ Prefix negation spellings, or 'Nothing' if the dialect has none and
  -- negations must be pushed into comparisons.
  , dNotCall :: Bool
  -- ^ Negation is written as a call, @not(e)@, rather than a prefix.
  , dNotLoose :: Bool
  -- ^ Prefix negation binds looser than a comparison, as in Python, R and
  -- VBScript: @not a == 1@ is @not (a == 1)@.
  , dQuote :: Char
  , dStrings :: Strings
  , dRef :: Text -> Text
  , dSelected :: Text -> Text -> Either Text Text
  -- ^ 'Left' names a choice value the dialect cannot spell.
  , dNotSelected :: Text -> Text -> Either Text Text
  , dAnswered :: Text -> Text
  , dUnanswered :: Text -> Text
  , dNormalise :: Expr -> Expr
  -- ^ Dialect-specific rewriting after parsing.
  }

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | A keyword or operator token. Word-like tokens must not run into an
-- identifier, so @and@ does not match the start of @android@.
tok :: Text -> Parser Text
tok t
  | T.all isIdentChar t = lexeme . try $ string' t <* notFollowedBy (satisfy isIdentChar)
  | otherwise = lexeme . try $ string t

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '.' || c == '-'

ident :: Parser Text
ident = T.pack <$> some (satisfy isIdentChar)

-- | How a dialect writes a quote, a backslash or a line break inside a
-- string. Reading undoes exactly what writing does.
data Strings
  = Verbatim
  -- ^ No escapes, as in XPath: a text containing the usual quote is
  -- written in the other kind, and one containing both cannot be written.
  | QuoteEscaped
  -- ^ A backslash before the enclosing quote stands for that quote; every
  -- other backslash is itself. A text ending in a backslash cannot be
  -- written.
  | CEscaped
  -- ^ Python and R: @\\\\@, @\\'@, @\\"@, @\\n@, @\\r@ and @\\t@.

-- | A string in either kind of quote.
quotedAs :: Strings -> Parser Text
quotedAs style = lexeme (q '\'' <|> q '"')
  where
    q c = T.pack <$> (char c *> manyTill (ch c) (char c))
    ch c = case style of
      Verbatim -> anySingle
      QuoteEscaped -> try (char '\\' *> char c) <|> anySingle
      CEscaped -> (char '\\' *> (escape <|> pure '\\')) <|> anySingle
    escape =
      choice
        [ '\\' <$ char '\\'
        , '\'' <$ char '\''
        , '"' <$ char '"'
        , '\n' <$ char 'n'
        , '\r' <$ char 'r'
        , '\t' <$ char 't'
        ]

-- | A text as a string in the given style, in the given quote where it
-- can be. 'Left' names what the style cannot write.
writeStr :: Strings -> Char -> Text -> Either Text Text
writeStr style q s = case style of
  Verbatim
    | not (T.any (== q) s) -> Right (wrap q s)
    | not (T.any (== other) s) -> Right (wrap other s)
    | otherwise -> Left "a text containing both kinds of quote"
  QuoteEscaped
    | "\\" `T.isSuffixOf` s -> Left "a text ending in a backslash"
    | otherwise -> Right (wrap q (T.replace (T.singleton q) (T.pack ['\\', q]) s))
  CEscaped -> Right (wrap q (T.concatMap esc s))
  where
    other = if q == '\'' then '"' else '\''
    wrap c t = T.singleton c <> t <> T.singleton c
    esc c
      | c == '\\' = "\\\\"
      | c == q = T.pack ['\\', q]
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | otherwise = T.singleton c

-- | A choice value written bare, as part of a name: it must be one the
-- dialect's reader takes back whole.
bareCode :: (Char -> Bool) -> Text -> Either Text Text
bareCode ok v
  | not (T.null v) && T.all ok v = Right v
  | otherwise = Left ("the choice value " <> v <> ", which is not a plain code")

isCodeChar :: Char -> Bool
isCodeChar c = isAlphaNum c || c == '_'

number :: Parser Double
number = lexeme . try $ do
  sign <- option id (negate <$ char '-')
  sign <$> (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))

literal :: Strings -> Parser Expr
literal style =
  choice
    [ ELit . LStr <$> quotedAs style
    , ELit . LNum <$> number
    , ELit (LBool True) <$ tok "true"
    , ELit (LBool False) <$ tok "false"
    ]

-- | Unknown function calls are kept by name so they round-trip.
call :: Parser Expr -> Parser Expr
call e = try $ do
  f <- lexeme ident
  args <- between (symbol "(") (symbol ")") (e `sepBy` symbol ",")
  pure (ECall f args)

exprP :: Dialect -> Parser Expr
exprP d = makeExprParser term table
  where
    term =
      choice
        ( map try (dAtoms d)
            ++ [ between (symbol "(") (symbol ")") (exprP d)
               , literal (dStrings d)
               , call (exprP d)
               ]
        )
        >>= \e -> foldr ($) e . reverse <$> many (choice (map try (dPostfix d)))
    ops op = [InfixL (EBin op <$ choice (map tok (spellings op)))]
    spellings op = sortOn (Down . T.length) (maybe [] id (lookup op (dOps d)))
    notOp = case dNot d of
      Just ts | not (dNotCall d) -> [Prefix (ENot <$ choice (map tok ts))]
      _ -> []
    arith = [ops Mul ++ ops Div, ops Add ++ ops Sub, concatMap ops [Le, Ge, Neq, Lt, Gt, Eq]]
    table
      | dNotLoose d = arith ++ [notOp, ops And, ops Or]
      | otherwise = [notOp] ++ arith ++ [ops And, ops Or]

-- | Parse a whole expression in the dialect, normalising
-- @ref != ''@ into 'EAnswered' and a @not(...)@ call into 'ENot'.
parseExpr :: Dialect -> Text -> Either String Expr
parseExpr d src =
  either (Left . errorBundlePretty) (Right . dNormalise d . normalise) $
    parse (sc *> exprP d <* eof) (T.unpack (dName d)) src

-- | Parse, or keep the source verbatim tagged with its dialect.
parseOrRaw :: Dialect -> Text -> Expr
parseOrRaw d src = either (const (ERaw (dName d) src)) id (parseExpr d src)

normalise :: Expr -> Expr
normalise = go
  where
    go e = case e of
      EBin Neq (ERef q) (ELit (LStr "")) -> EAnswered q
      EBin Eq (ERef q) (ELit (LStr "")) -> ENot (EAnswered q)
      ECall f [x] | T.toLower f == "not" -> go (ENot x)
      ENot (ENot x) -> go x
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      _ -> e

-- | Push a negation inward (flip comparisons, De Morgan) so dialects
-- without a @not@ operator can still express it.
pushNot :: Expr -> Maybe Expr
pushNot e = case e of
  EBin And a b -> EBin Or <$> pushNot a <*> pushNot b
  EBin Or a b -> EBin And <$> pushNot a <*> pushNot b
  EBin op a b -> (\o -> EBin o a b) <$> flipCmp op
  ENot x -> Just x
  ELit (LBool b) -> Just (ELit (LBool (not b)))
  _ -> Nothing
  where
    flipCmp op = lookup op [(Eq, Neq), (Neq, Eq), (Lt, Ge), (Ge, Lt), (Gt, Le), (Le, Gt)]

-- | Print in the dialect. 'Left' names the construct the dialect cannot
-- spell, so the caller can warn and drop it.
printExpr :: Dialect -> Expr -> Either Text Text
printExpr d = go (0 :: Int)
  where
    go p e = case e of
      ELit (LStr s) -> writeStr (dStrings d) (dQuote d) s
      ELit (LNum n) -> Right (showNum n)
      ELit (LBool b) -> Right (if b then "true" else "false")
      ERef q -> Right (dRef d q)
      ESelected q v -> negated p <$> dSelected d q v
      EAnswered q -> Right (negated p (dAnswered d q))
      ENot (EAnswered q) -> Right (negated p (dUnanswered d q))
      ENot (ESelected q v) -> negated p <$> dNotSelected d q v
      ENot x -> case dNot d of
        Just (t : _)
          | dNotCall d -> (\s -> t <> "(" <> s <> ")") <$> go 0 x
          | otherwise -> (\s -> paren (dNotLoose d && p > notPrec) (t <> s)) <$> go tight x
        _ -> maybe (Left "negation") (go p) (negIn x)
      EBin op a b -> do
        t <- maybe (Left (T.pack (show op))) Right (lookup op (dOps d) >>= headMay)
        let q = prec op
        sa <- go q a
        sb <- go (q + 1) b
        pure (paren (q < p) (sa <> " " <> t <> " " <> sb))
      ECall f xs -> (\as -> f <> "(" <> T.intercalate ", " as <> ")") <$> traverse (go 0) xs
      ERaw dn src
        | dn == dName d -> Right (paren (p > 0) src)
        | otherwise -> Left ("expression in " <> dn <> " syntax: " <> src)
    -- Where negation binds looser than a comparison, a test the dialect
    -- spells with a leading negation needs brackets inside one.
    negated p s = case dNot d of
      Just (t : _) | dNotLoose d, not (dNotCall d), p > notPrec, t `T.isPrefixOf` s -> paren True s
      _ -> s
    paren b s = if b then "(" <> s <> ")" else s
    -- A negation pushed all the way in, where the dialect spells negated
    -- membership and answer tests itself.
    negIn x = case x of
      EBin And a b -> EBin Or <$> negIn a <*> negIn b
      EBin Or a b -> EBin And <$> negIn a <*> negIn b
      ESelected {} -> Just (ENot x)
      EAnswered {} -> Just (ENot x)
      _ -> pushNot x
    headMay xs = case xs of
      (x : _) -> Just x
      [] -> Nothing

prec :: BinOp -> Int
prec op = case op of
  Or -> 2
  And -> 4
  Add -> 8
  Sub -> 8
  Mul -> 10
  Div -> 10
  _ -> 6

-- | A loose prefix negation sits between @and@ and the comparisons; the
-- operand of a negation is printed as tightly as anything binds.
notPrec, tight :: Int
notPrec = 5
tight = 14

showNum :: Double -> Text
showNum n
  | n == fromIntegral r = T.pack (show r)
  | otherwise = T.pack (showFFloat Nothing n "")
  where
    r = round n :: Integer

-- | Print for a writer; an expression the dialect cannot spell is dropped
-- with a warning naming where it was.
printLogic :: Dialect -> Text -> Maybe Expr -> Conv (Maybe Text)
printLogic _ _ Nothing = pure Nothing
printLogic d at (Just e) = case printExpr d e of
  Right t -> pure (Just t)
  Left why -> Nothing <$ warn at ("logic dropped, " <> dName d <> " cannot express " <> why)

-- | Replace the current-answer reference @.@ with a named question, for
-- dialects that have no self reference.
substSelf :: Text -> Expr -> Expr
substSelf q = go
  where
    go e = case e of
      ERef "." -> ERef q
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      ECall f xs -> ECall f (map go xs)
      _ -> e

-- | Question ids an expression reads.
refs :: Expr -> [Text]
refs e = case e of
  ERef q -> [q]
  ESelected q _ -> [q]
  EAnswered q -> [q]
  ENot x -> refs x
  EBin _ a b -> refs a ++ refs b
  ECall _ xs -> concatMap refs xs
  _ -> []

cmpOps :: [(BinOp, [Text])]
cmpOps =
  [ (Eq, ["="])
  , (Neq, ["!=", "<>"])
  , (Lt, ["<"])
  , (Le, ["<="])
  , (Gt, [">"])
  , (Ge, [">="])
  , (Add, ["+"])
  , (Sub, ["-"])
  , (Mul, ["*"])
  ]

-- | ODK XPath as written in XLSForm: @${q}@ references.
xlsform :: Dialect
xlsform =
  Dialect
    { dName = "xlsform"
    , dAtoms =
        [ do
            _ <- tok "selected"
            _ <- symbol "("
            q <- xref
            _ <- symbol ","
            v <- quotedAs Verbatim
            _ <- symbol ")"
            pure (ESelected q v)
        , ERef "." <$ lexeme (try (char '.' <* notFollowedBy digitChar))
        , ERef <$> xref
        ]
    , dPostfix = []
    , dOps = [(And, ["and"]), (Or, ["or"]), (Div, ["div"])] ++ cmpOps
    , dNot = Just ["not"]
    , dNotCall = True
    , dNotLoose = False
    , dQuote = '\''
    , dStrings = Verbatim
    , dRef = \q -> if q == "." then "." else "${" <> q <> "}"
    , dSelected = \q v -> (\t -> "selected(${" <> q <> "}, " <> t <> ")") <$> writeStr Verbatim '\'' v
    , dNotSelected = \q v -> (\t -> "not(selected(${" <> q <> "}, " <> t <> "))") <$> writeStr Verbatim '\'' v
    , dAnswered = \q -> "${" <> q <> "} != ''"
    , dUnanswered = \q -> "${" <> q <> "} = ''"
    , dNormalise = id
    }
  where
    xref = lexeme (string "${" *> (T.pack <$> someTill (satisfy (/= '}')) (char '}')))

-- | REDCap branching logic: @[q]@, and @[q(code)]@ for a checkbox option.
redcap :: Dialect
redcap =
  Dialect
    { dName = "redcap"
    , dAtoms =
        [ try $ do
            _ <- char '['
            q <- ident
            v <- between (char '(') (char ')') ident
            _ <- symbol "]"
            op <- (True <$ tok "=") <|> (False <$ (tok "<>" <|> tok "!="))
            b <- quotedAs QuoteEscaped <|> (T.pack . show <$> (L.decimal :: Parser Integer) <* sc)
            pure $ case (op, b) of
              (True, "1") -> ESelected q v
              (False, "0") -> ESelected q v
              _ -> ENot (ESelected q v)
        , ERef <$> lexeme (char '[' *> ident <* char ']')
        ]
    , dPostfix = []
    , dOps = [(And, ["and"]), (Or, ["or"]), (Div, ["/"]), (Neq, ["<>", "!="])] ++ cmpOps
    , dNot = Nothing
    , dNotCall = False
    , dNotLoose = False
    , dQuote = '\''
    , dStrings = QuoteEscaped
    , dRef = \q -> "[" <> q <> "]"
    , dSelected = \q v -> (\c -> "[" <> q <> "(" <> c <> ")] = '1'") <$> bareCode isIdentChar v
    , dNotSelected = \q v -> (\c -> "[" <> q <> "(" <> c <> ")] = '0'") <$> bareCode isIdentChar v
    , dAnswered = \q -> "[" <> q <> "] <> ''"
    , dUnanswered = \q -> "[" <> q <> "] = ''"
    , dNormalise = id
    }

-- | SurveyJS expressions: @{q}@, with @contains@, @notempty@, @empty@.
surveyjs :: Dialect
surveyjs =
  Dialect
    { dName = "surveyjs"
    , dAtoms =
        [ try $ do
            q <- sref
            choice
              [ EAnswered q <$ tok "notempty"
              , ENot (EAnswered q) <$ tok "empty"
              , ESelected q <$> (tok "contains" *> quotedAs QuoteEscaped)
              , ENot . ESelected q <$> (tok "notcontains" *> quotedAs QuoteEscaped)
              ]
        , ERef <$> sref
        ]
    , dPostfix = []
    , dOps =
        [ (And, ["and", "&&"])
        , (Or, ["or", "||"])
        , (Eq, ["=", "==", "equal"])
        , (Neq, ["!=", "<>", "notequal"])
        , (Div, ["/"])
        ]
          ++ cmpOps
    , dNot = Just ["!"]
    , dNotCall = False
    , dNotLoose = False
    , dQuote = '\''
    , dStrings = QuoteEscaped
    , dRef = \q -> "{" <> q <> "}"
    , dSelected = \q v -> (\t -> "{" <> q <> "} contains " <> t) <$> writeStr QuoteEscaped '\'' v
    , dNotSelected = \q v -> (\t -> "{" <> q <> "} notcontains " <> t) <$> writeStr QuoteEscaped '\'' v
    , dAnswered = \q -> "{" <> q <> "} notempty"
    , dUnanswered = \q -> "{" <> q <> "} empty"
    , dNormalise = id
    }
  where
    sref = lexeme (char '{' *> (T.pack <$> someTill (satisfy (/= '}')) (char '}')))

-- | LimeSurvey ExpressionScript. A multiple-choice option is its own
-- sub-question @q_code@ holding @"Y"@ when ticked.
limesurvey :: Dialect
limesurvey =
  Dialect
    { dName = "limesurvey"
    , dAtoms =
        [ try $ do
            _ <- tok "is_empty"
            q <- between (symbol "(") (symbol ")") lref
            pure (ENot (EAnswered q))
        , try $ do
            _ <- symbol "!"
            _ <- tok "is_empty"
            q <- between (symbol "(") (symbol ")") lref
            pure (EAnswered q)
        , ERef <$> lref
        ]
    , dPostfix = []
    , dOps =
        [ (And, ["&&", "and"])
        , (Or, ["||", "or"])
        , (Eq, ["==", "eq"])
        , (Neq, ["!=", "ne"])
        , (Lt, ["<", "lt"])
        , (Le, ["<=", "le"])
        , (Gt, [">", "gt"])
        , (Ge, [">=", "ge"])
        , (Add, ["+"])
        , (Sub, ["-"])
        , (Mul, ["*"])
        , (Div, ["/"])
        ]
    , dNot = Just ["!"]
    , dNotCall = False
    , dNotLoose = False
    , dQuote = '"'
    , dStrings = QuoteEscaped
    , dRef = \q -> q <> ".NAOK"
    , -- The code follows the last underscore, so it cannot hold one.
      dSelected = \q v -> (\c -> q <> "_" <> c <> ".NAOK == \"Y\"") <$> bareCode isAlphaNum v
    , dNotSelected = \q v -> (\c -> q <> "_" <> c <> ".NAOK != \"Y\"") <$> bareCode isAlphaNum v
    , dAnswered = \q -> "!is_empty(" <> q <> ".NAOK)"
    , dUnanswered = \q -> "is_empty(" <> q <> ".NAOK)"
    , dNormalise = lsSelected
    }
  where
    lref = lexeme . try $ do
      c0 <- satisfy (\c -> isAlpha c || c == '_')
      q <- T.pack . (c0 :) <$> many (satisfy (\c -> isAlphaNum c || c == '_'))
      _ <- optional (string ".NAOK")
      notFollowedBy (symbol "(")
      if q `elem` ["and", "or", "eq", "ne", "lt", "le", "gt", "ge", "true", "false"]
        then fail "keyword"
        else pure q

-- | @q_code == "Y"@ is how LimeSurvey tests a ticked multiple-choice
-- option; the option code follows the last underscore.
lsSelected :: Expr -> Expr
lsSelected = go
  where
    go e = case e of
      EBin Eq (ERef r) (ELit (LStr "Y")) | Just (q, v) <- split r -> ESelected q v
      EBin Neq (ERef r) (ELit (LStr "Y")) | Just (q, v) <- split r -> ENot (ESelected q v)
      ENot x -> ENot (go x)
      EBin o a b -> EBin o (go a) (go b)
      _ -> e
    split r = case T.breakOnEnd "_" r of
      (pre, v) | T.length pre > 1, not (T.null v) -> Just (T.dropEnd 1 pre, v)
      _ -> Nothing

-- | R as written in a Shiny/surveydown server: @input$q@ references,
-- @"b" %in% input$q@ for a ticked option and @sd_is_answered("q")@.
-- The surveydown writer wraps numeric comparisons in @as.numeric(input$q)@,
-- which reads back as a plain reference.
rlang :: Dialect
rlang =
  Dialect
    { dName = "r"
    , dAtoms =
        [ try $ do
            v <- quotedAs CEscaped
            _ <- tok "%in%"
            ESelected <$> rref <*> pure v
        , try $ do
            _ <- tok "sd_is_answered"
            EAnswered <$> between (symbol "(") (symbol ")") (quotedAs CEscaped)
        , ERef <$> rref
        ]
    , dPostfix = []
    , dOps =
        [ (And, ["&", "&&"])
        , (Or, ["|", "||"])
        , (Eq, ["=="])
        , (Neq, ["!="])
        , (Lt, ["<"])
        , (Le, ["<="])
        , (Gt, [">"])
        , (Ge, [">="])
        , (Add, ["+"])
        , (Sub, ["-"])
        , (Mul, ["*"])
        , (Div, ["/"])
        ]
    , dNot = Just ["!"]
    , dNotCall = False
    , dNotLoose = True
    , dQuote = '"'
    , dStrings = CEscaped
    , dRef = \q -> "input$" <> q
    , dSelected = \q v -> (\t -> t <> " %in% input$" <> q) <$> writeStr CEscaped '"' v
    , dNotSelected = \q v -> (\t -> "!(" <> t <> " %in% input$" <> q <> ")") <$> writeStr CEscaped '"' v
    , dAnswered = \q -> "sd_is_answered(\"" <> q <> "\")"
    , dUnanswered = \q -> "!sd_is_answered(\"" <> q <> "\")"
    , dNormalise = unNumeric
    }
  where
    rref = lexeme (string "input$" *> (T.pack <$> some (satisfy (\c -> isAlphaNum c || c == '_' || c == '.'))))
    unNumeric e = case e of
      ECall "as.numeric" [x@(ERef _)] -> x
      ENot x -> ENot (unNumeric x)
      EBin o a b -> EBin o (unNumeric a) (unNumeric b)
      ECall f xs -> ECall f (map unNumeric xs)
      _ -> e

-- | ODK XForms XPath, as in a @bind@: questions are node paths such as
-- @/data/grp/q1@ or @../q1@. The argument gives each question id's
-- absolute path for printing; parsing keeps a path's last step as the id.
-- XPath has no boolean literals, so @true()@ and @false()@ read as
-- 'LBool', but a writer must print them as calls itself.
xpath :: (Text -> Text) -> Dialect
xpath path =
  Dialect
    { dName = "xpath"
    , dAtoms =
        [ ELit (LBool True) <$ (tok "true" *> symbol "(" *> symbol ")")
        , ELit (LBool False) <$ (tok "false" *> symbol "(" *> symbol ")")
        , do
            _ <- tok "selected"
            _ <- symbol "("
            q <- xpref
            _ <- symbol ","
            v <- quotedAs Verbatim
            _ <- symbol ")"
            pure (ESelected q v)
        , ERef <$> xpref
        ]
    , dPostfix = []
    , dOps = [(And, ["and"]), (Or, ["or"]), (Div, ["div"])] ++ cmpOps
    , dNot = Just ["not"]
    , dNotCall = True
    , dNotLoose = False
    , dQuote = '\''
    , dStrings = Verbatim
    , dRef = ref
    , dSelected = \q v -> (\t -> "selected(" <> ref q <> ", " <> t <> ")") <$> writeStr Verbatim '\'' v
    , dNotSelected = \q v -> (\t -> "not(selected(" <> ref q <> ", " <> t <> "))") <$> writeStr Verbatim '\'' v
    , dAnswered = \q -> ref q <> " != ''"
    , dUnanswered = \q -> ref q <> " = ''"
    , dNormalise = id
    }
  where
    ref q = if q == "." then "." else path q
    xpref = lexeme . try $ try ("." <$ (char '.' <* notFollowedBy (satisfy (\c -> isAlphaNum c || c == '.' || c == '/')))) <|> nodePath
    -- An absolute path, or a relative one with at least two steps, so a
    -- bare word is left to the function-call parser.
    nodePath = do
      lead <- option False (True <$ char '/')
      steps <- (T.pack <$> some (satisfy isIdentChar)) `sepBy1` char '/'
      case reverse steps of
        (lst : rest) | lead || not (null rest) -> pure lst
        _ -> fail "not a node path"

-- | An identifier with no dots, starting with a letter or underscore.
plainName :: Parser Text
plainName = do
  c0 <- satisfy (\c -> isAlpha c || c == '_')
  T.pack . (c0 :) <$> many (satisfy (\c -> isAlphaNum c || c == '_'))

-- | A category name after a dot or inside a list; may start with a digit.
codeTok :: Parser Text
codeTok = T.pack <$> some (satisfy (\c -> isAlphaNum c || c == '_'))

notKeyword :: [Text] -> Text -> Parser Text
notKeyword kws t = if T.toLower t `elem` kws then fail "keyword" else pure t

-- | Forsta (Decipher) conditions are Python: @q1.r1@ is true when row
-- @r1@ of @q1@ is selected, @q1.any@ when anything is, and @q1.val@ is
-- the answer. Category tests use the row label, which writers map from the
-- choice value.
decipher :: Dialect
decipher =
  Dialect
    { dName = "decipher"
    , dAtoms = [dref]
    , dPostfix = []
    , dOps =
        [ (And, ["and"])
        , (Or, ["or"])
        , (Eq, ["=="])
        , (Neq, ["!="])
        , (Lt, ["<"])
        , (Le, ["<="])
        , (Gt, [">"])
        , (Ge, [">="])
        , (Add, ["+"])
        , (Sub, ["-"])
        , (Mul, ["*"])
        , (Div, ["/"])
        ]
    , dNot = Just ["not ", "not"]
    , dNotCall = False
    , dNotLoose = True
    , dQuote = '\''
    , dStrings = CEscaped
    , dRef = (<> ".val")
    , -- @q.any@ and @q.val@ are not category tests.
      dSelected = \q v -> (\c -> q <> "." <> c) <$> rowCode v
    , dNotSelected = \q v -> (\c -> "not " <> q <> "." <> c) <$> rowCode v
    , dAnswered = (<> ".any")
    , dUnanswered = \q -> "not " <> q <> ".any"
    , dNormalise = id
    }
  where
    rowCode v
      | v `elem` ["any", "val", "ival", "unsafe_val"] = Left ("the choice value " <> v <> ", which reads back as ." <> v)
      | otherwise = bareCode isCodeChar v
    dref = lexeme . try $ do
      q <- plainName >>= notKeyword ["and", "or", "not", "true", "false"]
      rest <- many (char '.' *> codeTok)
      notFollowedBy (char '(')
      case rest of
        [] -> pure (ERef q)
        [a]
          | a == "any" -> pure (EAnswered q)
          | a `elem` ["val", "ival", "unsafe_val"] -> pure (ERef q)
          | otherwise -> pure (ESelected q a)
        _ -> fail "cell reference"

-- | Dimensions / Unicom Intelligence mrScript routing conditions:
-- @q.ContainsAny({a, b})@, @IsEmpty(q)@, category literals @{a}@ and
-- VBScript operators. A numeric category value @1@ is the name @_1@.
mrscript :: Dialect
mrscript =
  Dialect
    { dName = "mrscript"
    , dAtoms =
        [ try $ do
            q <- plainName
            _ <- char '.'
            m <- T.toLower <$> plainName
            op <- case m of
              "containsany" -> pure Or
              "containsall" -> pure And
              _ -> fail "method"
            vs <- between (symbol "(") (symbol ")") catList
            pure (foldr1 (EBin op) [ESelected q v | v <- vs])
        , try $ do
            _ <- tok "IsEmpty"
            q <- between (symbol "(") (symbol ")") (lexeme plainName)
            pure (ENot (EAnswered q))
        , try $ do
            vs <- catList
            case vs of
              [v] -> pure (ELit (LStr v))
              _ -> fail "category set"
        , lexeme . try $ do
            q <- plainName >>= notKeyword ["and", "or", "not", "xor", "mod", "like", "true", "false"]
            notFollowedBy (char '.' <|> char '(')
            pure (ERef q)
        ]
    , dPostfix = []
    , dOps =
        [ (And, ["And"])
        , (Or, ["Or"])
        , (Eq, ["="])
        , (Neq, ["<>"])
        , (Lt, ["<"])
        , (Le, ["<="])
        , (Gt, [">"])
        , (Ge, [">="])
        , (Add, ["+"])
        , (Sub, ["-"])
        , (Mul, ["*"])
        , (Div, ["/"])
        ]
    , dNot = Just ["Not ", "Not"]
    , dNotCall = False
    , dNotLoose = True
    , dQuote = '"'
    , dStrings = QuoteEscaped
    , dRef = id
    , dSelected = \q v -> (\c -> q <> ".ContainsAny({" <> mrCode c <> "})") <$> bareCode isCodeChar v
    , dNotSelected = \q v -> (\c -> "Not " <> q <> ".ContainsAny({" <> mrCode c <> "})") <$> bareCode isCodeChar v
    , dAnswered = \q -> "Not IsEmpty(" <> q <> ")"
    , dUnanswered = \q -> "IsEmpty(" <> q <> ")"
    , dNormalise = id
    }
  where
    catList = between (symbol "{") (symbol "}") (lexeme (mrValue <$> codeTok) `sepBy1` symbol ",")
    mrCode v = case T.uncons v of
      Just (c, _) | isDigitC c -> "_" <> v
      _ -> v
    mrValue n = case T.uncons n of
      Just ('_', rest) | Just (c, _) <- T.uncons rest, isDigitC c -> rest
      _ -> n
    isDigitC c = c >= '0' && c <= '9'

-- | Blaise RULES conditions. An enumerated field is compared with a bare
-- category name (@q1 = Yes@, @q1 IN [Yes, No]@, which the reader resolves
-- against the field's categories); a SET field is tested with
-- @Cat IN q2@; @q = RESPONSE@ / @q = EMPTY@ test for an answer.
blaise :: Dialect
blaise =
  Dialect
    { dName = "blaise"
    , dAtoms =
        [ try $ do
            q <- lexeme bname
            _ <- tok "IN"
            vs <- between (symbol "[") (symbol "]") (lexeme bname `sepBy1` symbol ",")
            pure (foldr1 (EBin Or) [EBin Eq (ERef q) (ERef v) | v <- vs])
        , try $ do
            v <- lexeme codeTok
            _ <- tok "IN"
            q <- lexeme bname
            pure (ESelected q v)
        , try $ do
            q <- lexeme bname
            eq <- (True <$ tok "=") <|> (False <$ tok "<>")
            k <- (True <$ tok "RESPONSE") <|> (False <$ tok "EMPTY")
            pure $ if eq == k then EAnswered q else ENot (EAnswered q)
        , ERef <$> lexeme bname
        ]
    , dPostfix = []
    , dOps =
        [ (And, ["AND"])
        , (Or, ["OR"])
        , (Eq, ["="])
        , (Neq, ["<>"])
        , (Lt, ["<"])
        , (Le, ["<="])
        , (Gt, [">"])
        , (Ge, [">="])
        , (Add, ["+"])
        , (Sub, ["-"])
        , (Mul, ["*"])
        , (Div, ["/"])
        ]
    , dNot = Just ["NOT ", "NOT"]
    , dNotCall = False
    , dNotLoose = False
    , dQuote = '"'
    , dStrings = QuoteEscaped
    , dRef = id
    , dSelected = \q v -> (\c -> c <> " IN " <> q) <$> bareCode isCodeChar v
    , dNotSelected = \q v -> (\c -> "NOT (" <> c <> " IN " <> q <> ")") <$> bareCode isCodeChar v
    , dAnswered = (<> " = RESPONSE")
    , dUnanswered = (<> " = EMPTY")
    , dNormalise = id
    }
  where
    bname = try $ do
      n <- T.pack <$> ((:) <$> satisfy isAlpha <*> many (satisfy (\c -> isAlphaNum c || c == '_' || c == '.')))
      notKeyword ["and", "or", "not", "in", "empty", "response", "then", "true", "false"] n
