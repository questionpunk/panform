-- | A printable questionnaire in Markdown, for people rather than tools:
-- @##@ pages, numbered @###@ questions, @( )@ and @[ ]@ option lists,
-- tables for matrices, italic hints and "Shown if" lines in plain English.
-- Writer only; the lossless Markdown dialect is QuestionPunk Markdown.
module Panform.Format.Markdown (writeMarkdown, Names, surveyNames, english) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Format.TextUtil

-- | What the English printer needs: question numbers and choice labels.
data Names = Names (M.Map Text Text) (M.Map (Text, Text) Text)

-- | Questions numbered Q1, Q2, ... in document order, and their choice
-- labels by value.
surveyNames :: Form -> Names
surveyNames s =
  Names
    (M.fromListWith (\_ a -> a) (zip (map qId qs) ["Q" <> T.pack (show n) | n <- [1 :: Int ..]]))
    (M.fromList [((qId q, choiceValue c), txt (choiceLabel c)) | q <- qs, c <- qChoices (qType q)])
  where
    qs = questions s

matrixRows :: QType -> [Choice]
matrixRows = \case
  QMatrix _ rs _ -> rs
  _ -> []

qChoices :: QType -> [Choice]
qChoices = \case
  QChoice _ _ cs -> cs
  QRanking cs -> cs
  QMatrix _ _ cs -> cs
  _ -> []

esc :: Text -> Text
esc = T.concatMap $ \c -> case c of
  '\n' -> "<br>"
  _ | c `elem` ("\\*_`<[]|" :: String) -> T.pack ['\\', c]
  _ -> T.singleton c

writeMarkdown :: Form -> Conv BL.ByteString
writeMarkdown s@(Form meta items) = do
  warnTranslations "markdown" s
  mapM_ (const (warn "markdown" "survey id not shown")) (metaId meta)
  let names = surveyNames s
      qs = questions s
      intro =
        ["# " <> esc (txt t) | Just t <- [metaTitle meta]]
          ++ [esc (txt d) | Just d <- [metaDescription meta]]
          ++ ["*Questions marked \\* must be answered.*" | any qRequired qs]
  unless (null [() | q <- qs, c <- qChoices (qType q) ++ matrixRows (qType q), choiceValue c /= txt (choiceLabel c)]) $
    warn "markdown" "choice codes not shown; printable form only"
  pages <- forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    body <- concat <$> mapM (writeItem names) kids
    pure $ case g of
      Nothing | n == 1 -> body
      _ -> ("## " <> maybe ("Page " <> T.pack (show n)) (esc . txt) (g >>= groupTitle)) : shownIf names (g >>= groupRelevant) ++ body
  pure . BL.fromStrict . TE.encodeUtf8 $ T.intercalate "\n\n" (intro ++ concat pages) <> "\n"

shownIf :: Names -> Maybe Expr -> [Text]
shownIf names e = ["**Shown if** " <> esc (english names x) <> "." | Just x <- [e]]

writeItem :: Names -> Item -> Conv [Text]
writeItem names = \case
  ItemNote _ t rel -> pure (("> " <> esc (txt t)) : shownIf names rel)
  ItemRaw (Format "markdown") (String t) -> pure [t]
  ItemRaw (Format f) _ -> [] <$ warn "markdown" ("raw " <> f <> " content dropped")
  ItemGroup g -> do
    kids <- concat <$> mapM (writeItem names) (groupItems g)
    pure $
      ["#### " <> maybe (groupId g) (esc . txt) (groupTitle g)]
        ++ shownIf names (groupRelevant g)
        ++ ["*Repeat for each entry.*" | groupKind g == Repeat]
        ++ kids
  ItemQuestion q -> writeQuestion names q

writeQuestion :: Names -> Question -> Conv [Text]
writeQuestion names@(Names nums _) q = do
  when (isJust (qDefault q)) $ warn (qId q) "default value not shown"
  let v = qValidation q
      num = M.findWithDefault (qId q) (qId q) nums
      heading = "### " <> num <> ". " <> esc (txt (qLabel q)) <> (if qRequired q then " \\*" else "")
      validLine = case v of
        Just val
          | parts@(_ : _) <- catMaybes [range val, esc . english names <$> valExpr val] ->
              ["**Answer must be** " <> T.intercalate "; " parts <> maybe "" (\m -> " (" <> esc (txt m) <> ")") (valMessage val) <> "."]
        _ -> []
  pure $
    [heading]
      ++ ["*" <> esc (txt h) <> "*" | Just h <- [qHint q]]
      ++ shownIf names (qRelevant q)
      ++ validLine
      ++ body
  where
    range val = case (valMin val, valMax val) of
      (Just lo, Just hi) -> Just ("between " <> showNumber lo <> " and " <> showNumber hi)
      (Just lo, Nothing) -> Just ("at least " <> showNumber lo)
      (Nothing, Just hi) -> Just ("at most " <> showNumber hi)
      _ -> Nothing
    blank = "______________________________"
    body = case qType q of
      QText k -> case k of
        ShortText -> [blank]
        LongText -> [blank <> blank]
        Email -> [blank <> " (email)"]
        Url -> [blank <> " (web address)"]
        Phone -> [blank <> " (phone)"]
      QNumber Integer -> ["__________ (whole number)"]
      QNumber Decimal -> ["__________ (number)"]
      QDate Date -> ["____-__-__ (date)"]
      QDate Time -> ["__:__ (time)"]
      QDate DateTime -> ["____-__-__ __:__ (date and time)"]
      QChoice sel style cs ->
        ["*Choose one from the list.*" | style == Dropdown, sel == Single]
          ++ ["*Choose all that apply.*" | sel == Multiple]
          ++ [T.intercalate "\n" (map (optionLine (if sel == Single then "- ( ) " else "- [ ] ")) cs)]
      QRanking cs -> ["*Number the options in order, 1 = first.*", T.intercalate "\n" (map (optionLine "- \\_\\_ ") cs)]
      QMatrix sel rows cols ->
        [ T.intercalate "\n" $
            ("|  | " <> T.intercalate " | " (map (esc . txt . choiceLabel) cols) <> " |")
              : ("|---|" <> T.concat (map (const ":-:|") cols))
              : ["| " <> esc (txt (choiceLabel r)) <> " | " <> T.intercalate " | " (map (const (if sel == Single then "( )" else "[ ]")) cols) <> " |" | r <- rows]
        ]
      QScale lo hi l h -> [T.unwords (maybe [] (pure . esc . txt) l ++ ["( ) " <> T.pack (show n) | n <- [lo .. hi]] ++ maybe [] (pure . esc . txt) h)]
      QBoolean -> ["( ) Yes   ( ) No"]
      QFile -> ["\\[Attach a file\\]"]
      QGeo -> ["Latitude __________   Longitude __________"]
      QCalculate e -> ["**Calculated:** " <> esc (english names e)]
      QHidden -> ["*Filled in automatically.*"]
      QOther _ -> [blank]
    optionLine prefix c =
      prefix <> esc (txt (choiceLabel c)) <> (if choiceOther c then " ________" else "") <> (if choiceExclusive c then " *(only this)*" else "")

------------------------------------------------------------------------------
-- Logic in English

-- | A condition as a sentence fragment: @Q1 is "Yes" and Q2 includes "API"@.
english :: Names -> Expr -> Text
english (Names nums labels) = go Top
  where
    name q
      | q == "." = "the answer"
      | otherwise = M.findWithDefault q q nums
    lbl q v = "\"" <> M.findWithDefault v (q, v) labels <> "\""
    go ctx e = case e of
      EBin And a b -> paren (ctx == InOr || ctx == Operand) (go InAnd a <> " and " <> go InAnd b)
      EBin Or a b -> paren (ctx == InAnd || ctx == Operand) (go InOr a <> " or " <> go InOr b)
      EBin Eq (ERef q) (ELit l) -> name q <> " is " <> litFor q l
      EBin Neq (ERef q) (ELit l) -> name q <> " is not " <> litFor q l
      EBin op a b -> paren (ctx == Operand) (go Operand a <> " " <> opWord op <> " " <> go Operand b)
      ESelected q v -> name q <> " includes " <> lbl q v
      ENot (ESelected q v) -> name q <> " does not include " <> lbl q v
      EAnswered q -> name q <> " is answered"
      ENot (EAnswered q) -> name q <> " is not answered"
      ENot x -> "not (" <> go Top x <> ")"
      ERef q -> name q
      ELit l -> litFor "" l
      ECall f xs -> f <> "(" <> T.intercalate ", " (map (go Top) xs) <> ")"
      ERaw d src -> d <> " `" <> src <> "`"
    litFor q = \case
      LStr v -> lbl q v
      LNum n -> let v = showNumber n in maybe v (\l -> "\"" <> l <> "\"") (M.lookup (q, v) labels)
      LBool b -> if b then "true" else "false"
    paren b t = if b then "(" <> t <> ")" else t
    opWord = \case
      Eq -> "is"
      Neq -> "is not"
      Lt -> "is less than"
      Le -> "is at most"
      Gt -> "is more than"
      Ge -> "is at least"
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"
      And -> "and"
      Or -> "or"

data Ctx = Top | InAnd | InOr | Operand
  deriving (Eq)
