-- | Namespace-agnostic XML helpers for the XML-based standards formats
-- (Triple-S, DDI, ODM, QTI). Reading goes through xml-conduit and matches
-- element and attribute names by local name; writing uses a tiny tree with
-- attributes kept in the order given, so output is stable and readable.
module Panform.XmlKit
  ( Node (..)
  , el
  , leaf
  , renderXml
  , parseXml
  , X.Element
  , localName
  , children
  , childrenAny
  , child
  , descendants
  , allDescendants
  , attr
  , attrMaybe
  , langOf
  , textOf
  , deepText
  , deepTextExcept
  , exactText
  , trimLayout
  , nonBlank
  , readLangs
  , langNodes
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Panform.AST (I18n (..), Lang)
import Panform.Conv
import qualified Text.XML as X

------------------------------------------------------------------------------
-- Writing

-- | An element (name, attributes in order, children) or a text node.
data Node = El Text [(Text, Text)] [Node] | Tx Text
  deriving (Eq, Show)

el :: Text -> [(Text, Text)] -> [Node] -> Node
el = El

-- | An element holding only text.
leaf :: Text -> [(Text, Text)] -> Text -> Node
leaf n as t = El n as [Tx t]

-- | UTF-8 with a declaration. Elements whose children are all elements are
-- indented one per line; anything holding text is written inline so that
-- no whitespace is added to content.
renderXml :: Node -> BL.ByteString
renderXml root = BL.fromStrict (TE.encodeUtf8 (T.concat ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" : go 0 root ++ ["\n"])))
  where
    go :: Int -> Node -> [Text]
    go _ (Tx t) = [escText t]
    go d (El n as cs)
      | null cs = [open n as <> "/>"]
      | all isEl cs = [open n as <> ">"] ++ concatMap (\c -> ("\n" <> ind (d + 1)) : go (d + 1) c) cs ++ ["\n" <> ind d <> "</" <> n <> ">"]
      | otherwise = [open n as <> ">"] ++ concatMap (inline) cs ++ ["</" <> n <> ">"]
    inline (Tx t) = [escText t]
    inline (El n as cs)
      | null cs = [open n as <> "/>"]
      | otherwise = [open n as <> ">"] ++ concatMap inline cs ++ ["</" <> n <> ">"]
    open n as = "<" <> n <> T.concat [" " <> k <> "=\"" <> escAttr v <> "\"" | (k, v) <- as]
    ind d = T.replicate d "  "
    isEl = \case
      El {} -> True
      Tx _ -> False

escText :: Text -> Text
escText = T.concatMap $ \case
  '\r' -> "&#13;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  c -> T.singleton c

escAttr :: Text -> Text
escAttr = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\n' -> "&#10;"
  '\t' -> "&#9;"
  '\r' -> "&#13;"
  c -> T.singleton c

------------------------------------------------------------------------------
-- Reading

-- | Parse a document and return its root element. A Latin-1 or
-- Windows-1252 declaration (common in older Triple-S files) is honoured;
-- anything else is read as UTF-8.
parseXml :: Text -> BL.ByteString -> Conv X.Element
parseXml what bs = X.documentRoot <$> (either (Left . show) Right (X.parseText X.def (decode bs)) `orFail` what)
  where
    decode b = case declaredEncoding b of
      Just e
        | e `elem` ["windows-1252", "cp1252", "x-cp1252"] -> TL.pack (map cp1252 (BL.unpack b))
        | e `elem` ["iso-8859-1", "iso_8859-1", "latin1", "latin-1", "l1"] -> TL.pack (map (toEnum . fromIntegral) (BL.unpack b))
      _ -> TLE.decodeUtf8With (\_ _ -> Just '\xFFFD') (dropBom b)
    dropBom b = if BL.take 3 b == BL.pack [0xEF, 0xBB, 0xBF] then BL.drop 3 b else b

-- | The @encoding@ named by the XML declaration at the very start of the
-- document, lower-cased.
declaredEncoding :: BL.ByteString -> Maybe Text
declaredEncoding bs = do
  let start = T.pack (map (toEnum . fromIntegral) (BL.unpack (BL.take 200 bs)))
  decl <- T.stripPrefix "<?xml" start
  let (inside, close) = T.breakOn "?>" decl
  if T.null close then Nothing else Just ()
  rest <- case T.breakOn "encoding" inside of
    (_, r) | T.null r -> Nothing
    (_, r) -> Just (T.dropWhile isSpace (T.drop 8 r))
  value <- T.stripPrefix "=" rest
  let v = T.dropWhile isSpace value
  q <- T.uncons v
  case q of
    (c, more) | c `elem` ['"', '\''] -> Just (T.toLower (T.takeWhile (/= c) more))
    _ -> Nothing

-- | A Windows-1252 byte as a character: 0x80 to 0x9F are mostly
-- punctuation (the euro sign, curly quotes, dashes); the five bytes the
-- code page leaves undefined keep their Latin-1 meaning, as browsers do.
cp1252 :: Word8 -> Char
cp1252 w
  | w >= 0x80 && w <= 0x9F = T.index c1 (fromIntegral w - 0x80)
  | otherwise = toEnum (fromIntegral w)
  where
    c1 = T.pack "\x20AC\x81\x201A\x0192\x201E\x2026\x2020\x2021\x02C6\x2030\x0160\x2039\x0152\x8D\x017D\x8F\x90\x2018\x2019\x201C\x201D\x2022\x2013\x2014\x02DC\x2122\x0161\x203A\x0153\x9D\x017E\x0178"

localName :: X.Element -> Text
localName = X.nameLocalName . X.elementName

children :: Text -> X.Element -> [X.Element]
children n e = [c | c <- elems e, localName c == n]

-- | Children with any of the given local names, in document order.
childrenAny :: [Text] -> X.Element -> [X.Element]
childrenAny ns e = [c | c <- elems e, localName c `elem` ns]

child :: Text -> X.Element -> Maybe X.Element
child n = listToMaybe . children n

elems :: X.Element -> [X.Element]
elems e = [c | X.NodeElement c <- X.elementNodes e]

descendants :: Text -> X.Element -> [X.Element]
descendants n e = concatMap go (elems e)
  where
    go c = [c | localName c == n] ++ descendants n c

-- | Every element below this one, in document order.
allDescendants :: X.Element -> [X.Element]
allDescendants = concatMap (\c -> c : allDescendants c) . elems

attr :: Text -> X.Element -> Text
attr n = maybe "" id . attrMaybe n

-- | An attribute by local name, so @redcap:Variable@ and @Variable@ match.
attrMaybe :: Text -> X.Element -> Maybe Text
attrMaybe n e = listToMaybe [v | (k, v) <- M.toList (X.elementAttributes e), X.nameLocalName k == n]

-- | The element's own @xml:lang@.
langOf :: X.Element -> Maybe Text
langOf e = listToMaybe [v | (k, v) <- M.toList (X.elementAttributes e), X.nameLocalName k == "lang", X.namePrefix k == Just "xml" || X.nameNamespace k == Just "http://www.w3.org/XML/1998/namespace"]

-- | Direct text content, whitespace-trimmed.
textOf :: X.Element -> Text
textOf e = T.strip (T.concat (mapMaybe t (X.elementNodes e)))
  where
    t (X.NodeContent c) = Just c
    t _ = Nothing

-- | All text below the element with runs of whitespace collapsed; @<br/>@
-- becomes a space.
deepText :: X.Element -> Text
deepText = deepTextExcept []

-- | Text exactly as written when the element holds only text (what the
-- writers emit), so whitespace survives a round trip; mixed content is
-- flattened with 'deepText'.
exactText :: X.Element -> Text
exactText e
  | null (elems e) = T.concat [c | X.NodeContent c <- X.elementNodes e]
  | otherwise = deepText e

-- | Remove indentation around a value (whitespace at either end that
-- includes a line break), keeping spaces that are part of it.
trimLayout :: Text -> Text
trimLayout t =
  let (lead, rest) = T.span isSpace t
      (body, trail) = (T.dropWhileEnd isSpace rest, T.takeWhileEnd isSpace rest)
   in (if T.any (== '\n') lead then "" else lead) <> body <> (if T.any (== '\n') trail then "" else trail)

-- | 'deepText', skipping child elements with the given local names.
deepTextExcept :: [Text] -> X.Element -> Text
deepTextExcept skip = T.unwords . T.words . go
  where
    go e = T.concat (map node (X.elementNodes e))
    node (X.NodeContent c) = c
    node (X.NodeElement c)
      | localName c `elem` skip = " "
      | otherwise = " " <> go c <> " "
    node _ = ""

nonBlank :: Text -> Maybe Text
nonBlank t = if T.all isSpace t then Nothing else Just t

-- | One text repeated per language (DDI @labl@, ODM @TranslatedText@): the
-- element without @xml:lang@, or in the default language, is the default;
-- the rest are translations. 'Nothing' when there are no elements.
readLangs :: Maybe Lang -> [X.Element] -> Maybe I18n
readLangs defLang es = case (filter isDefault es, es) of
  (d : _, _) -> Just (mk d)
  ([], d : _) -> Just (mk d)
  ([], []) -> Nothing
  where
    isDefault e = maybe True (\l -> Just l == defLang) (langOf e)
    mk d = I18n (exactText d) (M.fromListWith (\_ old -> old) [(l, exactText e) | e <- es, Just l <- [langOf e], Just l /= defLang, Just l /= langOf d])

-- | The inverse of 'readLangs': the default text untagged (or tagged with
-- the default language when it is known), then one element per translation.
langNodes :: Maybe Lang -> Text -> [(Text, Text)] -> I18n -> [Node]
langNodes defLang n as t =
  leaf n (as ++ [("xml:lang", l) | Just l <- [defLang], not (M.null trans)]) (i18nDefault t)
    : [leaf n (as ++ [("xml:lang", l)]) x | (l, x) <- M.toList trans]
  where
    -- a "translation" into the default language is the default text
    trans = M.filterWithKey (\l _ -> Just l /= defLang) (i18nTranslations t)
