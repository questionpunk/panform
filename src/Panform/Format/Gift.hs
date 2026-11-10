-- | Moodle GIFT: one question per blank-line-separated block,
-- @::title:: text {answers}@, @~@ for an option and @=@ for the right one,
-- @{}@ essay, @{#}@ numeric, @{T}@ true/false, @$CATEGORY:@ for grouping.
-- GIFT is a quiz format: surveys have no right answers, so the writer marks
-- none and the reader keeps any it finds in @gift:correct@.
module Panform.Format.Gift (readGift, writeGift) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Result (..), Value (..), fromJSON, toJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toUpper)
import Data.List (isPrefixOf, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Numeric (showFFloat)
import Panform.AST
import Panform.Conv
import Panform.Format.TextUtil

------------------------------------------------------------------------------
-- Reading

readGift :: BL.ByteString -> Conv Form
readGift bs = do
  let ls = filter (not . isComment) (T.lines (T.replace "\r" "" (TE.decodeUtf8With TE.lenientDecode (BL.toStrict bs))))
  items <- group Nothing [] <$> readBlocks 1 (blocks ls)
  let scored = [() | ItemQuestion q <- allItems items, any (`M.member` qAttrs q) ["gift:correct", "gift:weights"]]
  unless (null scored) $ warn "gift" "right answers kept as gift:correct; scoring is not a survey concept"
  pure (Form emptyMeta items)
  where
    isComment l = "//" `T.isPrefixOf` T.stripStart l
    -- Loose items before the first category stay at the top level.
    group cur acc = \case
      [] -> close cur acc
      Left c : rest -> close cur acc ++ group (Just c) [] rest
      Right i : rest -> group cur (i : acc) rest
    close Nothing acc = reverse acc
    close (Just c) acc = [ItemGroup (Group c Page Nothing Nothing (reverse acc) M.empty)]

blocks :: [Text] -> [[Text]]
blocks ls = case dropWhile blank ls of
  [] -> []
  rest -> let (b, more) = break blank rest in b : blocks more
  where
    blank = T.all isSpace

-- | A category switch ('Left') or an item.
readBlocks :: Int -> [[Text]] -> Conv [Either Text Item]
readBlocks _ [] = pure []
readBlocks n (b : bs) = case b of
  (l : rest)
    | Just c <- T.stripPrefix "$CATEGORY:" (T.stripStart l) ->
        (Left (T.strip c) :) <$> readBlocks n (if null rest then bs else rest : bs)
  _ -> do
    i <- readQuestion n (T.unpack (T.intercalate "\n" b))
    (Right i :) <$> readBlocks (n + 1) bs

data Answer = Answer
  { ansRight :: Bool
  , ansWeight :: Maybe Double
  , ansText :: Text
  , ansFeedback :: Bool
  }

readQuestion :: Int -> String -> Conv Item
readQuestion n src = do
  let (title, body) = case breakStr "::" (trim src) of
        ("", Just rest) -> case breakStr "::" rest of
          (t, Just b) -> (Just (unescRaw t), b)
          _ -> (Nothing, trim src)
        _ -> (Nothing, trim src)
      qid = maybe ("q" <> T.pack (show n)) id title
      (fmt, body') = markup (trim body)
      fmtAttr = [("gift:format", String f) | Just f <- [fmt]]
  case breakStr "{" body' of
    (pre, Nothing) -> pure (ItemNote qid (plain (unesc pre)) Nothing)
    (pre, Just rest) -> do
      let (ans, after) = case breakStr "}" rest of
            (a, Just af) -> (a, af)
            (a, Nothing) -> (a, "")
          label
            | all isSpace after = unesc pre
            | otherwise = T.stripEnd (unesc pre) <> " _____ " <> T.stripStart (unesc after)
      unless (all isSpace after) $ warn qid "missing-word question read with a blank in its text"
      (qt, attrs) <- answers qid (trim ans)
      let range = case lookup "gift:range" attrs of
            Just v | Just [a, b] <- fromJSONMaybe v -> Just (Validation (Just a) (Just b) Nothing Nothing)
            _ -> Nothing
      pure . ItemQuestion $
        (question qid label qt) {qValidation = range, qAttrs = M.fromList (fmtAttr ++ filter ((/= "gift:range") . fst) attrs)}
  where
    markup s = case s of
      '[' : rest | (m, ']' : after) <- break (== ']') rest, m `elem` ["html", "moodle", "plain", "markdown"] -> (Just (T.pack m), trim after)
      _ -> (Nothing, s)

answers :: Text -> String -> Conv (QType, [(Text, Value)])
answers qid src
  | null body = pure (QText LongText, [])
  | '#' : num <- body = numeric (trim num)
  | map toUpper (trim (fst (breakStr "#" body))) `elem` ["T", "TRUE"] = pure (QBoolean, [("gift:correct", "T")])
  | map toUpper (trim (fst (breakStr "#" body))) `elem` ["F", "FALSE"] = pure (QBoolean, [("gift:correct", "F")])
  | otherwise = do
      let as = map answer (splitAnswers body)
      when (any ansFeedback as) $ warn qid "answer feedback dropped"
      when (matching as) $ warn qid "matching question read as a matrix"
      pure (choices as)
  where
    (body, general) = breakStr "####" src
    -- Matching pairs all begin with @=@; an option after @~@ that holds
    -- @->@ is text.
    matching as = all ansRight as && any (isJust . snd . breakStr "->" . T.unpack . ansText) as
    choices as
      | matching as =
          let pairs = [(T.strip l, T.strip (unesc r)) | a <- as, (l0, Just r) <- [breakStr "->" (T.unpack (ansText a))], let l = unesc l0]
              cols = nub (map snd pairs)
           in ( QMatrix Single [choice r r | (r, _) <- pairs] [choice c c | c <- cols]
              , [("gift:correct", toJSON (M.fromList pairs))]
              )
      | all ansRight as = (QText ShortText, [("gift:correct", toJSON (map ansText as))])
      | otherwise =
          let multi = not (any ansRight as) && any (maybe False (> 0) . ansWeight) as
              weights = map ansWeight as
              uniform = case nub weights of
                [Just w] -> w > 0
                _ -> False
           in ( QChoice (if multi then Multiple else Single) Radio [choice (ansText a) (ansText a) | a <- as]
              , [("gift:correct", toJSON [ansText a | a <- as, ansRight a]) | any ansRight as]
                  ++ [("gift:weights", toJSON (M.fromList [(ansText a, w) | a <- as, Just w <- [ansWeight a]])) | any isJust weights, not (multi && uniform)]
              )
    numeric s = do
      when (isJust general) $ warn qid "general feedback dropped"
      pure $ case breakStr ".." s of
        (lo, Just hi) | Just a <- readD lo, Just b <- readD hi -> (QNumber Decimal, [("gift:range", toJSON [a, b])])
        _
          | null s -> (QNumber Decimal, [])
          | otherwise -> (QNumber Decimal, [("gift:correct", String (T.pack s))])
    readD x = case reads (trim x) :: [(Double, String)] of
      [(d, "")] -> Just d
      _ -> Nothing

fromJSONMaybe :: Value -> Maybe [Double]
fromJSONMaybe v = case fromJSON v of
  Success x -> Just x
  Error _ -> Nothing

-- | Split an answer list at each unescaped @=@ or @~@ that starts an answer.
splitAnswers :: String -> [(Char, String)]
splitAnswers = go . dropWhile isSpace
  where
    go (m : rest) | m `elem` ("=~" :: String) = let (a, more) = breakU (`elem` ("=~" :: String)) rest in (m, a) : go more
    go _ = []

answer :: (Char, String) -> Answer
answer (m, raw) =
  let (w, rest) = case trim raw of
        '%' : r | (ws, '%' : after) <- break (== '%') r, [(d, "")] <- reads ws -> (Just d, after)
        r -> (Nothing, r)
      (t, fb) = breakStr "#" rest
   in Answer (m == '=') w (T.strip (unesc t)) (isJust fb)

-- | Break at the first unescaped occurrence of a string.
breakStr :: String -> String -> (String, Maybe String)
breakStr pat = go []
  where
    go acc s | pat `isPrefixOf` s = (reverse acc, Just (drop (length pat) s))
    go acc ('\\' : c : rest) = go (c : '\\' : acc) rest
    go acc (c : rest) = go (c : acc) rest
    go acc [] = (reverse acc, Nothing)

-- | Break at the first unescaped character matching; the rest keeps it.
breakU :: (Char -> Bool) -> String -> (String, String)
breakU p = go []
  where
    go acc ('\\' : c : rest) = go (c : '\\' : acc) rest
    go acc (c : rest) | p c = (reverse acc, c : rest)
    go acc (c : rest) = go (c : acc) rest
    go acc [] = (reverse acc, [])

unescRaw :: String -> Text
unescRaw = T.pack . go
  where
    go ('\\' : 'n' : rest) = '\n' : go rest
    go ('\\' : c : rest) = c : go rest
    go (c : rest) = c : go rest
    go [] = []

unesc :: String -> Text
unesc = T.strip . T.pack . go
  where
    go ('\\' : 'n' : rest) = '\n' : go rest
    go ('\\' : c : rest) = c : go rest
    go (c : rest) = c : go rest
    go [] = []

trim :: String -> String
trim = T.unpack . T.strip . T.pack

------------------------------------------------------------------------------
-- Writing

writeGift :: Form -> Conv BL.ByteString
writeGift s@(Form meta items) = do
  warnTranslations "gift" s
  unless (null [() | t <- surveyTexts s, T.any (== '\r') (i18nDefault t)]) $ warn "gift" "carriage returns written as line breaks"
  mapM_ (const (warn "gift" "survey title dropped")) (metaTitle meta)
  mapM_ (const (warn "gift" "survey description dropped")) (metaDescription meta)
  pages <- forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    mapM_ (const (warn (pageId n g) "page title dropped")) (g >>= groupTitle)
    mapM_ (const (warn (pageId n g) "page condition dropped")) (g >>= groupRelevant)
    flat <- flattenGroups kids
    bs <- concat <$> mapM writeItem flat
    -- Items before any category stay outside one; later loose items need
    -- a category of their own, or they would join the one before.
    case g of
      Nothing | n == 1 -> pure bs
      Nothing -> ("$CATEGORY: " <> pageId n g) : bs <$ warn (pageId n g) "loose items put in a category"
      Just _ -> pure (("$CATEGORY: " <> pageId n g) : bs)
  pure . BL.fromStrict . TE.encodeUtf8 $ T.intercalate "\n\n" (concat pages) <> "\n"

writeItem :: Item -> Conv [Text]
writeItem = \case
  ItemNote i t rel -> do
    mapM_ (const (warn i "display logic dropped")) rel
    pure ["::" <> esc i <> ":: " <> esc (T.strip (txt t))]
  ItemRaw (Format "gift") (String t) -> pure [t]
  ItemRaw (Format f) _ -> [] <$ warn "gift" ("raw " <> f <> " content dropped")
  ItemGroup g -> flattenGroups [ItemGroup g] >>= fmap concat . mapM writeItem
  ItemQuestion q -> writeQuestion q

writeQuestion :: Question -> Conv [Text]
writeQuestion q = do
  let at = qId q
  mapM_ (const (warn at "hint dropped")) (qHint q)
  when (qRequired q) $ warn at "required flag dropped"
  mapM_ (const (warn at "display logic dropped")) (qRelevant q)
  mapM_ (const (warn at "default value dropped")) (qDefault q)
  when (M.member "gift:correct" (qAttrs q) && not (isBool (qType q))) $ warn at "right answer dropped; all options written as ~"
  let numericRange = case (qType q, qValidation q) of
        (QNumber _, Just (Validation (Just lo) (Just hi) Nothing Nothing)) -> Just (lo, hi)
        _ -> Nothing
  when (isJust (qValidation q) && isNothing numericRange) $ warn at "validation dropped"
  case qType q of
    QText k -> do
      unless (k == LongText) $ warn at "text question written as an essay"
      one "{}"
    QNumber k -> do
      when (k == Integer) $ warn at "whole-number restriction dropped"
      one (maybe "{#}" (\(lo, hi) -> "{#" <> showNumber lo <> ".." <> showNumber hi <> "}") numericRange)
    QBoolean -> case M.lookup "gift:correct" (qAttrs q) of
      Just (String "F") -> one "{F}"
      Just (String "T") -> one "{T}"
      _ -> warn at "true/false needs a right answer; T written" >> one "{T}"
    QChoice _ _ [] -> warn at "choice question without options written as an essay" >> one "{}"
    QChoice sel style cs -> do
      when (style == Dropdown) $ warn at "dropdown style dropped"
      checkChoices cs
      one (options sel weights cs)
    QScale lo hi l h -> do
      warn at "scale written as a single choice"
      one (options Single Nothing (scaleChoices lo hi l h))
    QRanking cs -> do
      warn at "ranking written as a multiple choice"
      checkChoices cs
      one (options Multiple Nothing cs)
    QMatrix sel rows cols -> do
      warn at "matrix expanded into one question per row"
      concat <$> mapM writeQuestion (matrixAsQuestions q {qAttrs = M.empty} sel rows cols)
    QDate _ -> warn at "date written as an essay" >> one "{}"
    QFile -> warn at "file upload written as an essay" >> one "{}"
    QGeo -> warn at "geopoint written as an essay" >> one "{}"
    QOther t -> warn at ("type " <> t <> " written as an essay") >> one "{}"
    QHidden -> [] <$ warn at "hidden field dropped"
    QCalculate _ -> [] <$ warn at "calculated field dropped"
  where
    one a = pure ["::" <> esc (qId q) <> ":: " <> markup <> esc (T.strip (txt (qLabel q))) <> " " <> a]
    markup = case M.lookup "gift:format" (qAttrs q) of
      Just (String f) -> "[" <> f <> "]"
      _ -> ""
    weights = case M.lookup "gift:weights" (qAttrs q) of
      Just v | Success m <- fromJSON v -> Just m
      _ -> Nothing
    isBool = \case
      QBoolean -> True
      _ -> False
    checkChoices cs = do
      when (any (\c -> choiceValue c /= txt (choiceLabel c)) cs) $ warn (qId q) "choice codes dropped; labels are the values"
      when (any choiceOther cs) $ warn (qId q) "other free-text box dropped"
      when (any choiceExclusive cs) $ warn (qId q) "exclusive option flag dropped"

-- | Options, with the source's own weights when it had any
-- (@gift:weights@), else equal weights for a multiple choice.
options :: Selection -> Maybe (M.Map Text Double) -> [Choice] -> Text
options sel weights cs = "{\n" <> T.concat [(let p = prefix (txt (choiceLabel c)) in p <> escOption (p == "~") (T.strip (txt (choiceLabel c)))) <> "\n" | c <- cs] <> "}"
  where
    -- After a bare @~@ a leading @%n%@ would be taken for a weight; a
    -- backslash keeps it as text.
    escOption bare t =
      let e = esc t
       in if bare && "%" `T.isPrefixOf` e then "\\" <> e else e
    prefix l = case (weights, sel) of
      (Just m, _) -> maybe "~" (\w -> "~%" <> pct w <> "%") (M.lookup (T.strip l) m)
      (Nothing, Single) -> "~"
      (Nothing, Multiple) -> "~%" <> pct (100 / fromIntegral (max 1 (length cs))) <> "%"
    pct w = T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0') . T.pack $ showFFloat (Just 5) (w :: Double) ""

-- | GIFT escapes; a carriage return becomes a line break, since the reader
-- ignores carriage returns.
esc :: Text -> Text
esc = T.concatMap escChar . T.replace "\r" "\n" . T.replace "\r\n" "\n"
  where
    escChar c = case c of
      '\n' -> "\\n"
      _ | c `elem` ("~=#{}:\\" :: String) -> T.pack ['\\', c]
      _ -> T.singleton c
