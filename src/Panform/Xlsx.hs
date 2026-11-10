-- | Just enough OOXML spreadsheet to read and write XLSForm: named sheets of
-- text cells. Pure: the zip timestamps are fixed so output is reproducible.
--
-- Reading is bounded, so a small hostile workbook cannot ask for unbounded
-- memory: a column past Excel's last one (XFD, 16,384), more than
-- 'maxCells' cells, or XML parts that inflate past 'maxEntryBytes' each or
-- 'maxTotalBytes' together are refused with an error.
module Panform.Xlsx
  ( Sheet
  , readXlsx
  , writeXlsx
  ) where

import Codec.Archive.Zip (Archive, Entry, addEntryToArchive, eUncompressedSize, emptyArchive, findEntryByPath, fromArchive, fromEntry, toArchiveOrFail, toEntry)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiUpper, isDigit, isHexDigit, ord)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex, showHex)
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.XML as X

-- | A sheet name and its rows of cells; ragged rows are allowed.
type Sheet = (Text, [[Text]])

------------------------------------------------------------------------------
-- Reading

-- | Excel's column limit: the last column is XFD.
maxColumns :: Int
maxColumns = 16384

-- | The most cells one workbook may hold, counting the blanks before a
-- cell in its row.
maxCells :: Int
maxCells = 5000000

-- | The most one XML part may inflate to, and all the parts read together.
maxEntryBytes, maxTotalBytes :: Int64
maxEntryBytes = 32 * 1024 * 1024
maxTotalBytes = 96 * 1024 * 1024

readXlsx :: BL.ByteString -> Either String [Sheet]
readXlsx bs = do
  ar <- toArchiveOrFail bs
  (wb, b1) <- xml maxTotalBytes ar "xl/workbook.xml"
  (rels, b2) <- xml b1 ar "xl/_rels/workbook.xml.rels"
  (shared, b3) <- case findEntryByPath "xl/sharedStrings.xml" ar of
    Nothing -> Right ([], b2)
    Just _ -> (\(e, b) -> (sharedStrings e, b)) <$> xml b2 ar "xl/sharedStrings.xml"
  let sharedMap = M.fromList (zip [0 :: Int ..] shared)
      targets = M.fromList [(attr "Id" e, attr "Target" e) | e <- descendants "Relationship" rels]
      sheets = [(attr "name" e, relId e) | e <- descendants "sheet" wb]
      relId e = fromMaybe "" (lookupAttrLocal "id" e)
      step (acc, bytes, cells) (name, rid) = do
        let target = M.findWithDefault "" rid targets
            path = if "/" `T.isPrefixOf` target then T.drop 1 target else "xl/" <> target
        (doc, bytes') <- xml bytes ar (T.unpack path)
        (rows, cells') <- sheetRows cells sharedMap doc
        pure ((name, rows) : acc, bytes', cells')
  (out, _, _) <- foldM step ([], b3, maxCells) sheets
  pure (reverse out)

-- | One XML part, given how many bytes may still be inflated; returns what
-- is left of that allowance.
xml :: Int64 -> Archive -> FilePath -> Either String (X.Element, Int64)
xml budget ar path = case findEntryByPath path ar of
  Nothing -> Left ("xlsx: missing " <> path)
  Just e -> do
    raw <- inflate (min maxEntryBytes budget) path e
    doc <- either (Left . show) Right (X.parseLBS X.def raw)
    pure (X.documentRoot doc, budget - BL.length raw)

-- | An entry's content, refused when it is larger than the limit. The size
-- the archive declares is checked first, then the bytes actually produced
-- (the declared size can lie), reading at most one byte past the limit.
inflate :: Int64 -> FilePath -> Entry -> Either String BL.ByteString
inflate limit path e
  | fromIntegral (eUncompressedSize e) > limit = Left tooBig
  | otherwise = case forceBytes (BL.take (limit + 1) (fromEntry e)) of
      Left err -> Left ("xlsx: cannot decompress " <> path <> ": " <> err)
      Right b
        | BL.length b > limit -> Left tooBig
        | otherwise -> Right b
  where
    tooBig = "xlsx: " <> path <> " is too large (the parts of a workbook may inflate to " <> show (maxEntryBytes `div` (1024 * 1024)) <> " MiB each and " <> show (maxTotalBytes `div` (1024 * 1024)) <> " MiB together)"

-- | The zip library inflates lazily and reports a corrupt stream by
-- throwing from pure code. Forcing the bytes here turns that into a 'Left'.
-- The result depends only on the argument, so this is still a function.
forceBytes :: BL.ByteString -> Either String BL.ByteString
forceBytes b = unsafePerformIO $ do
  r <- try (evaluate (BL.length b))
  case r of
    Right _ -> pure (Right b)
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Left (show e))
{-# NOINLINE forceBytes #-}

sharedStrings :: X.Element -> [Text]
sharedStrings root = [stringText si | si <- children "si" root]

-- | The text of a string item: its @t@ elements, in or out of rich-text
-- runs. Phonetic runs (@rPh@, furigana) are a reading aid beside the
-- text, not part of it.
stringText :: X.Element -> Text
stringText = unescapeX . T.concat . map textOf . ts
  where
    ts e = concat [if n == "t" then [c] else if n == "rPh" then [] else ts c | X.NodeElement c <- X.elementNodes e, let n = X.nameLocalName (X.elementName c)]

-- | A sheet's rows, given how many cells may still be built; returns what
-- is left of that allowance.
sheetRows :: Int -> M.Map Int Text -> X.Element -> Either String ([[Text]], Int)
sheetRows budget0 sharedMap root = (\(rows, left) -> (reverse rows, left)) <$> foldM row ([], budget0) (descendants "row" root)
  where
    row (acc, budget) r = do
      cells <- place 0 [] (children "c" r)
      let width = if null cells then 0 else maximum (map fst cells) + 1
          m = M.fromList (reverse cells)
      if width > budget
        then Left ("xlsx: more than " <> show maxCells <> " cells in the workbook")
        else Right ([M.findWithDefault "" i m | i <- [0 .. width - 1]] : acc, budget - width)
    -- A cell sits in the column its reference names; one without a
    -- reference follows the cell before it.
    place _ acc [] = Right acc
    place next acc (c : cs) = do
      i <- if T.null (attr "r" c) then Right next else colIndex (attr "r" c)
      if i >= maxColumns
        then Left ("xlsx: a row has more than " <> show maxColumns <> " columns")
        else place (i + 1) ((i, value c) : acc) cs
    value c = case attr "t" c of
      "s" -> fromMaybe "" (readInt (vText c) >>= (`M.lookup` sharedMap))
      "inlineStr" -> stringText c
      "b" -> if vText c == "1" then "TRUE" else "FALSE"
      _ -> cleanNumber (vText c)
    vText c = T.concat (map textOf (children "v" c))
    readInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing
    -- Integers stored as floats ("1.0") are read back as "1".
    cleanNumber t = fromMaybe t (T.stripSuffix ".0" t)

-- | "B7" -> 1. Columns run from A to XFD; anything else is refused.
colIndex :: Text -> Either String Int
colIndex ref
  | T.null letters || T.length letters > 3 || not (T.all isDigit digits) || n > maxColumns = Left ("xlsx: cell reference " <> show (T.take 20 ref) <> " is outside the sheet (columns run from A to XFD)")
  | otherwise = Right (n - 1)
  where
    (letters, digits) = T.span isAsciiUpper ref
    n = T.foldl' (\acc c -> acc * 26 + ord c - ord 'A' + 1) 0 letters

------------------------------------------------------------------------------
-- Writing

writeXlsx :: [Sheet] -> BL.ByteString
writeXlsx sheets = fromArchive (foldr addEntryToArchive emptyArchive entries)
  where
    entries =
      [ entry "[Content_Types].xml" contentTypes
      , entry "_rels/.rels" rootRels
      , entry "xl/workbook.xml" workbook
      , entry "xl/_rels/workbook.xml.rels" workbookRels
      ]
        ++ [entry ("xl/worksheets/sheet" <> show n <> ".xml") (sheetXml rows) | (n, (_, rows)) <- numbered]
    numbered = zip [1 :: Int ..] sheets
    entry p t = toEntry p 0 (BL.fromStrict (TE.encodeUtf8 t))
    contentTypes =
      T.concat $
        [ decl
        , "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
        , "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
        , "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
        , "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
        ]
          ++ [ "<Override PartName=\"/xl/worksheets/sheet" <> tshow n <> ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
             | (n, _) <- numbered
             ]
          ++ ["</Types>"]
    rootRels =
      decl
        <> "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>"
        <> "</Relationships>"
    workbook =
      T.concat $
        [ decl
        , "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets>"
        ]
          ++ ["<sheet name=\"" <> esc name <> "\" sheetId=\"" <> tshow n <> "\" r:id=\"rId" <> tshow n <> "\"/>" | (n, (name, _)) <- numbered]
          ++ ["</sheets></workbook>"]
    workbookRels =
      T.concat $
        [decl, "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"]
          ++ [ "<Relationship Id=\"rId" <> tshow n <> "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet" <> tshow n <> ".xml\"/>"
             | (n, _) <- numbered
             ]
          ++ ["</Relationships>"]
    sheetXml rows =
      T.concat $
        [decl, "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>"]
          ++ [ "<row r=\"" <> tshow r <> "\">" <> T.concat (zipWith (cell r) [0 ..] cells) <> "</row>"
             | (r, cells) <- zip [1 :: Int ..] rows
             ]
          ++ ["</sheetData></worksheet>"]
    cell _ _ "" = ""
    cell r c v = "<c r=\"" <> colName c <> tshow r <> "\" t=\"inlineStr\"><is><t xml:space=\"preserve\">" <> esc (protect v) <> "</t></is></c>"
    decl = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"

colName :: Int -> Text
colName n
  | n < 26 = T.singleton (toEnum (ord 'A' + n))
  | otherwise = colName (n `div` 26 - 1) <> colName (n `mod` 26)

------------------------------------------------------------------------------
-- XML helpers (namespace-agnostic)

children :: Text -> X.Element -> [X.Element]
children n e = [c | X.NodeElement c <- X.elementNodes e, X.nameLocalName (X.elementName c) == n]

descendants :: Text -> X.Element -> [X.Element]
descendants n e = concatMap go [c | X.NodeElement c <- X.elementNodes e]
  where
    go c = [c | X.nameLocalName (X.elementName c) == n] ++ descendants n c

attr :: Text -> X.Element -> Text
attr n = fromMaybe "" . lookupAttrLocal n

lookupAttrLocal :: Text -> X.Element -> Maybe Text
lookupAttrLocal n e = case [v | (k, v) <- M.toList (X.elementAttributes e), X.nameLocalName k == n] of
  (v : _) -> Just v
  [] -> Nothing

textOf :: X.Element -> Text
textOf e = T.concat (mapMaybe t (X.elementNodes e))
  where
    t (X.NodeContent c) = Just c
    t _ = Nothing

esc :: Text -> Text
esc = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  -- XML parsers turn a raw carriage return into a newline, and reject the
  -- other C0 controls; write them as references so text survives exactly.
  '\r' -> "&#13;"
  c | c < ' ' && c /= '\n' && c /= '\t' -> "_x" <> T.justifyRight 4 '0' (T.pack (showHex (fromEnum c) "")) <> "_"
  c -> T.singleton c

-- | Text that already looks like an @_xHHHH_@ escape would be decoded on
-- reading (@_x0041_@ as @A@). Its underscore is written as @_x005F_@, the
-- escape for an underscore, so the text reads back as it was.
protect :: Text -> Text
protect t = case T.breakOn "_x" t of
  (_, rest) | T.null rest -> t
  _ -> T.concat (go t)
  where
    go x = case T.breakOn "_x" x of
      (pre, rest)
        | T.null rest -> [pre]
        | isEscape rest -> pre : "_x005F_" : go (T.drop 1 rest)
        | otherwise -> pre : "_x" : go (T.drop 2 rest)
    isEscape rest = T.length (T.take 7 rest) == 7 && T.index rest 6 == '_' && T.all isHexDigit (T.take 4 (T.drop 2 rest))

-- | Excel writes characters XML cannot hold as @_xHHHH_@.
unescapeX :: Text -> Text
unescapeX t = case T.breakOn "_x" t of
  (_, rest) | T.null rest -> t
  _ -> T.concat (go t)
  where
    go x = case T.breakOn "_x" x of
      (pre, rest)
        | T.null rest -> [pre]
        | T.length (T.take 7 rest) == 7
        , T.index rest 6 == '_'
        , [(n, "")] <- readHex (T.unpack (T.take 4 (T.drop 2 rest))) ->
            pre : T.singleton (toEnum n) : go (T.drop 7 rest)
        | otherwise -> pre : "_x" : go (T.drop 2 rest)

tshow :: Show a => a -> Text
tshow = T.pack . show

