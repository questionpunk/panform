-- | The logic language of QuestionPunk Markdown: plain words, no sigils.
--
-- > used = yes and features has api
-- > nps <= 6 and why is answered
-- > not (country = "United States") or age >= $min_age
--
-- A bare word on the left of a comparison is a question; on the right it is
-- a value. @$name@ refers to a question on the right-hand side. Values that
-- are not bare words are double-quoted.
module Panform.Expr.QP
  ( parseQP
  , printQP
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (showFFloat)
import Panform.AST hiding (choice)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` ("_.-" :: String)

keywords :: [Text]
keywords = ["and", "or", "not", "has", "is", "answered", "empty", "true", "false"]

keyword :: Text -> Parser ()
keyword k = lexeme . try $ string k *> notFollowedBy (satisfy isWordChar)

word :: Parser Text
word = lexeme . try $ do
  w <- T.pack <$> some (satisfy isWordChar)
  if w `elem` keywords then fail ("keyword " <> T.unpack w) else pure w

quoted :: Parser Text
quoted = lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))

isNumeric :: Text -> Bool
isNumeric w = case reads (T.unpack w) :: [(Double, String)] of
  [(_, "")] -> T.all (\c -> isDigit c || c `elem` ("-." :: String)) w
  _ -> False

-- | A right-hand value: a number, a quoted string, @$question@, a call or a
-- bare word; parentheses group arithmetic.
value :: Parser Expr
value =
  choice
    [ between (symbol "(") (symbol ")") (arith value)
    , ERef <$> (char '$' *> word)
    , try (ECall <$> word <*> between (symbol "(") (symbol ")") (expr `sepBy` symbol ","))
    , ELit . LStr <$> quoted
    , ELit (LBool True) <$ keyword "true"
    , ELit (LBool False) <$ keyword "false"
    , lit <$> word
    ]
  where
    lit w = case reads (T.unpack w) :: [(Double, String)] of
      [(n, "")] | T.all (\c -> isDigit c || c `elem` ("-." :: String)) w -> ELit (LNum n)
      _ -> ELit (LStr w)

cmpOps :: [(Text, BinOp)]
cmpOps = [("<=", Le), (">=", Ge), ("!=", Neq), ("=", Eq), ("<", Lt), (">", Gt)]

atom :: Parser Expr
atom =
  choice
    [ between (symbol "(") (symbol ")") expr
    , do
        q <- try (word >>= \w -> if isNumeric w then fail "number" else pure w)
        choice
          [ do
              args <- between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
              let call = ECall q args
              option call (do op <- choice [o <$ symbol s' | (s', o) <- cmpOps]; EBin op call <$> arith value)
          , ESelected q <$> (keyword "has" *> (either id id <$> bareOrQuoted))
          , try (EAnswered q <$ (keyword "is" *> keyword "answered"))
          , try (ENot (EAnswered q) <$ (keyword "is" *> keyword "empty"))
          , do
              op <- choice [o <$ symbol s | (s, o) <- cmpOps]
              EBin op (ERef q) <$> arith value
          , pure (ERef q)
          ]
    , value
    ]
  where
    bareOrQuoted = (Right <$> quoted) <|> (Left <$> word)

-- | Arithmetic over values and question references.
arith :: Parser Expr -> Parser Expr
arith term =
  makeExprParser
    term
    [ [InfixL (EBin Mul <$ symbol "*"), InfixL (EBin Div <$ symbol "/")]
    , [InfixL (EBin Add <$ symbol "+"), InfixL (EBin Sub <$ lexeme (try (char '-' <* notFollowedBy (satisfy isDigit))))]
    ]

expr :: Parser Expr
expr =
  makeExprParser
    (arith atom)
    [ [Prefix (ENot <$ keyword "not")]
    , [InfixL (EBin And <$ keyword "and")]
    , [InfixL (EBin Or <$ keyword "or")]
    ]

parseQP :: Text -> Either String Expr
parseQP src = either (Left . errorBundlePretty) (Right . tidy) (parse (sc *> expr <* eof) "logic" src)
  where
    tidy e = case e of
      ENot (ENot x) -> tidy x
      ENot x -> ENot (tidy x)
      EBin o a b -> EBin o (tidy a) (tidy b)
      _ -> e

-- | Print; 'Left' names what has no spelling (a raw expression from
-- another dialect).
printQP :: Expr -> Either Text Text
printQP = go 0
  where
    go :: Int -> Expr -> Either Text Text
    go p e = case e of
      EBin op a b | op `elem` [And, Or] -> do
        let q = if op == Or then 1 else 2
        sa <- go q a
        sb <- go (q + 1) b
        pure (paren (p > q) (sa <> (if op == Or then " or " else " and ") <> sb))
      -- A comparison, a test or a negation inside arithmetic keeps its
      -- brackets: the reader would otherwise take the arithmetic as its
      -- right-hand side.
      EBin op (ERef q) b | Just s <- lookup op (map swap cmpOps) -> (\v -> paren (p > 3) (q <> " " <> s <> " " <> v)) <$> rhs b
      EBin op a b | Just s <- lookup op arithOps -> do
        let q = arithPrec op
        sa <- go q a
        sb <- go (q + 1) b
        pure (paren (p > q) (sa <> " " <> s <> " " <> sb))
      EBin op l@(ECall _ _) b | Just s <- lookup op (map swap cmpOps) -> (\x v -> paren (p > 3) (x <> " " <> s <> " " <> v)) <$> go 6 l <*> rhs b
      EBin op _ _ -> Left ("comparison without a question on the left (" <> T.pack (show op) <> ")")
      ESelected q v -> Right (paren (p > 3) (q <> " has " <> val v))
      EAnswered q -> Right (paren (p > 3) (q <> " is answered"))
      ENot (EAnswered q) -> Right (paren (p > 3) (q <> " is empty"))
      ENot x -> (\s -> paren (p > 3) ("not " <> s)) <$> go 3 x
      ERef q -> Right q
      -- Outside a comparison a bare word is a question, so a string is quoted.
      ELit (LStr v) -> Right (quote v)
      ELit _ -> rhs e
      ECall f xs -> (\as -> f <> "(" <> T.intercalate ", " as <> ")") <$> traverse arg xs
      ERaw "questionpunk" src -> Right (paren (p > 0) src)
      ERaw d src -> Left ("expression in " <> d <> " syntax: " <> src)
    -- Inside a call a bare word is a question, so strings are always quoted.
    arg = \case
      ELit (LStr v) -> Right (quote v)
      e -> go 0 e
    rhs = rhsP 0
    rhsP :: Int -> Expr -> Either Text Text
    rhsP p = \case
      ERef q -> Right ("$" <> q)
      e@(ECall _ _) -> go 6 e
      ELit (LStr s) -> Right (val s)
      ELit (LNum n) -> Right (num n)
      ELit (LBool b) -> Right (if b then "true" else "false")
      EBin op a b | Just s <- lookup op arithOps -> do
        let q = arithPrec op
        x <- rhsP q a
        y <- rhsP (q + 1) b
        pure (paren (p > q) (x <> " " <> s <> " " <> y))
      other -> Left ("unsupported value " <> T.pack (show other))
    arithOps = [(Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/")]
    -- Multiplication binds tighter than addition, and both group to the left,
    -- so the right operand of an equal-precedence operator keeps its brackets.
    arithPrec op = if op `elem` [Mul, Div] then 5 else 4 :: Int
    swap (a, b) = (b, a)
    paren b s = if b then "(" <> s <> ")" else s
    num n
      | n == fromIntegral (round n :: Integer) = T.pack (show (round n :: Integer))
      | otherwise = T.pack (showFFloat Nothing n "")
    val s
      | not (T.null s), T.all isWordChar s, s `notElem` keywords, not (looksNumeric s) = s
      | otherwise = quote s
    -- The reader takes Haskell character escapes, so a backslash, a quote and
    -- the line-breaking characters are written as escapes. A condition is one
    -- line of the file.
    quote s = "\"" <> T.concatMap esc s <> "\""
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c -> T.singleton c
    looksNumeric s = T.all (\c -> isDigit c || c `elem` ("-." :: String)) s
