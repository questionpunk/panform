-- | Moodle's Aiken format: a one-line question, lettered options
-- (@A. @ or @A) @) and a mandatory @ANSWER: X@ line, blank lines between.
-- It holds single-choice questions and nothing else: no ids, pages, logic
-- or codes. The answer letter is kept in @aiken:answer@.
module Panform.Format.Aiken (readAiken, writeAiken) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiUpper, isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Panform.AST
import Panform.Conv
import Panform.Format.TextUtil

------------------------------------------------------------------------------
-- Reading

readAiken :: BL.ByteString -> Conv Form
readAiken bs = do
  let ls = T.lines (T.replace "\r" "" (TE.decodeUtf8With TE.lenientDecode (BL.toStrict bs)))
  qs <- forM (zip [1 :: Int ..] (blocks ls)) $ \(n, b) -> readQuestion ("q" <> T.pack (show n)) b
  unless (null qs) $ warn "aiken" "answer letters kept as aiken:answer; scoring is not a survey concept"
  pure (Form emptyMeta (map ItemQuestion qs))

blocks :: [Text] -> [[Text]]
blocks ls = case dropWhile blank ls of
  [] -> []
  rest -> let (b, more) = break blank rest in b : blocks more
  where
    blank = T.all isSpace

readQuestion :: Text -> [Text] -> Conv Question
readQuestion qid b = do
  let (stem, rest) = break (isJust . option) b
      opts = mapMaybe option rest
      answer = [T.strip a | l <- rest, Just a <- [T.stripPrefix "ANSWER:" (T.stripStart l)]]
  when (null opts) $ warn qid "question without options read as a single choice with none"
  pure
    (question qid (T.strip (T.unwords (map T.strip stem))) (QChoice Single Radio [choice o o | (_, o) <- opts]))
      { qAttrs = M.fromList [("aiken:answer", String a) | (a : _) <- [answer]]
      }

-- | @A. text@ or @A) text@.
option :: Text -> Maybe (Char, Text)
option l = case T.unpack (T.stripStart l) of
  c : d : ' ' : rest | isAsciiUpper c, d `elem` (".)" :: String) -> Just (c, T.strip (T.pack rest))
  _ -> Nothing

------------------------------------------------------------------------------
-- Writing

writeAiken :: Form -> Conv BL.ByteString
writeAiken s@(Form meta items) = do
  warnTranslations "aiken" s
  mapM_ (const (warn "aiken" "survey title dropped")) (metaTitle meta)
  mapM_ (const (warn "aiken" "survey description dropped")) (metaDescription meta)
  unless (null [() | ItemGroup _ <- items]) $ warn "aiken" "pages and groups dropped"
  flat <- concat <$> mapM flatten items
  unless (null [() | ItemQuestion _ <- flat]) $ warn "aiken" "question ids dropped; read back as q1, q2, ..."
  out <- concat <$> mapM writeItem flat
  pure . BL.fromStrict . TE.encodeUtf8 $ T.concat [t <> "\n" | t <- out]
  where
    flatten = \case
      ItemGroup g -> do
        mapM_ (const (warn (groupId g) "group condition dropped")) (groupRelevant g)
        concat <$> mapM flatten (groupItems g)
      i -> pure [i]

writeItem :: Item -> Conv [Text]
writeItem = \case
  ItemQuestion q -> writeQuestion q
  ItemNote i _ _ -> [] <$ warn i "note dropped"
  ItemRaw (Format f) _ -> [] <$ warn "aiken" ("raw " <> f <> " content dropped")
  ItemGroup _ -> pure []

writeQuestion :: Question -> Conv [Text]
writeQuestion q = do
  let at = qId q
  case qType q of
    QChoice _ _ [] -> [] <$ warn at "choice question without options dropped"
    QChoice sel _ cs -> do
      when (sel == Multiple) $ warn at "multiple choice written as single choice"
      block cs
    QScale lo hi l h -> warn at "scale written as a single choice" >> block (scaleChoices lo hi l h)
    QMatrix _ rows cols -> do
      warn at "matrix expanded into one question per row"
      concat <$> mapM writeQuestion (matrixAsQuestions q {qAttrs = M.empty} Single rows cols)
    _ -> [] <$ warn at "dropped; aiken holds single-choice questions only"
  where
    block cs = do
      let at = qId q
      mapM_ (const (warn at "hint dropped")) (qHint q)
      mapM_ (const (warn at "display logic dropped")) (qRelevant q)
      when (qRequired q) $ warn at "required flag dropped"
      mapM_ (const (warn at "default value dropped")) (qDefault q)
      when (any (\c -> choiceValue c /= txt (choiceLabel c)) cs) $ warn at "choice codes dropped; labels are the values"
      when (any choiceOther cs || any choiceExclusive cs) $ warn at "other/exclusive option flags dropped"
      when (length cs > 26) $ warn at "options beyond Z dropped"
      let kept = zip ['A' .. 'Z'] cs
          letters = map fst kept
          texts = txt (qLabel q) : map (txt . choiceLabel . snd) kept
          stem = oneLine (txt (qLabel q))
      -- Aiken has one line per text and no escapes.
      when (any (\t -> oneLine t /= t) texts) $ warn at "line breaks and repeated or surrounding spaces written as single spaces"
      when (isJust (option stem) || "ANSWER:" `T.isPrefixOf` stem) $ warn at "question text starts like an option or answer line; an Aiken reader will not take it for the question"
      answer <- case M.lookup "aiken:answer" (qAttrs q) of
        Just (String a) | [c] <- T.unpack a, c `elem` letters -> pure a
        _ -> "A" <$ warn at "aiken needs a right answer; A written"
      pure $
        [oneLine (txt (qLabel q))]
          ++ [T.singleton l <> ". " <> oneLine (txt (choiceLabel c)) | (l, c) <- kept]
          ++ ["ANSWER: " <> answer, ""]

oneLine :: Text -> Text
oneLine = T.unwords . T.words
