-- | CSV/TSV as lists of header-keyed rows, pure in both directions.
module Panform.Csv
  ( Row
  , readRows
  , writeRows
  , col
  , parseCells
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import Data.Char (ord)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type Row = M.Map Text Text

-- | Rows keyed by the (trimmed, lower-cased) header. Cell text is kept
-- exactly; callers trim the structural columns they compare. A UTF-8 BOM is
-- ignored. Nothing is dropped: a file that is not UTF-8, a header naming
-- one column twice, or a value past the last named column is an error.
readRows :: Char -> BL.ByteString -> Either String [Row]
readRows sep bs = do
  case TE.decodeUtf8' (BL.toStrict (dropBom bs)) of
    Left e -> Left ("not valid UTF-8 (" <> show e <> "); save the file as UTF-8")
    Right _ -> Right ()
  case parseCells sep bs of
    [] -> Right []
    (hdr : rest) -> do
      let keys = map (T.toLower . T.strip) hdr
          counts = M.fromListWith (+) [(k, 1 :: Int) | k <- keys, not (T.null k)]
      case [k | (k, n) <- M.toList counts, n > 1] of
        [] -> Right ()
        dups -> Left ("the header names " <> T.unpack (T.intercalate ", " dups) <> " more than once")
      let toRow (n, r) = case filter (not . T.null) (drop (length keys) r) of
            [] -> Right (M.fromList (zip keys r))
            _ -> Left ("record " <> show n <> " has a value past the last named column")
      mapM toRow [(n, r) | (n, r) <- zip [2 :: Int ..] rest, any (not . T.null) r]

dropBom :: BL.ByteString -> BL.ByteString
dropBom bs = if BL.take 3 bs == BL.pack [0xEF, 0xBB, 0xBF] then BL.drop 3 bs else bs

-- | Cells of a CSV/TSV file, parsed the forgiving way spreadsheet programs
-- do: a quote inside an unquoted cell is kept as text, and a quoted cell
-- that runs into more text keeps that text. A UTF-8 BOM is ignored.
parseCells :: Char -> BL.ByteString -> [[Text]]
parseCells sep bs = filter (/= [""]) (records (T.unpack text))
  where
    text = TE.decodeUtf8Lenient (BL.toStrict (dropBom bs))
    records [] = []
    records cs = let (r, rest) = record [] cs in r : records rest
    -- One record: fields until an unquoted line break.
    record acc cs = case field cs of
      (f, rest) -> case rest of
        (c : more) | c == sep -> record (f : acc) more
        ('\r' : '\n' : more) -> (reverse (f : acc), more)
        ('\n' : more) -> (reverse (f : acc), more)
        ('\r' : more) -> (reverse (f : acc), more)
        _ -> (reverse (f : acc), [])
    field ('"' : cs) = quoted [] cs
    field cs = unquoted [] cs
    unquoted acc cs = case cs of
      (c : _) | c == sep || c == '\n' || c == '\r' -> (T.pack (reverse acc), cs)
      (c : more) -> unquoted (c : acc) more
      [] -> (T.pack (reverse acc), [])
    quoted acc cs = case cs of
      ('"' : '"' : more) -> quoted ('"' : acc) more
      ('"' : more@(c : _)) | c /= sep && c /= '\n' && c /= '\r' -> unquoted ('"' : acc) more
      ('"' : more) -> (T.pack (reverse acc), more)
      (c : more) -> quoted (c : acc) more
      [] -> (T.pack (reverse acc), [])

-- | Rows in the given column order; missing cells are blank.
writeRows :: Char -> [Text] -> [Row] -> BL.ByteString
writeRows sep hdr rows =
  C.encodeWith opts (map (map TE.encodeUtf8) (hdr : [[M.findWithDefault "" h r | h <- hdr] | r <- rows]))
  where
    opts = C.defaultEncodeOptions {C.encDelimiter = fromIntegral (ord sep), C.encUseCrLf = False}

col :: Text -> Row -> Text
col k = M.findWithDefault "" k
