-- | Real-world corpus check: every file under @<corpus>/<format>/@ is read
-- with the format its directory names, then written and re-read through every
-- format that has both a reader and a writer. The corpus is a separate
-- repository (its files carry their own licences): @$PANFORM_CORPUS@, or
-- @../panform-corpus@ next to this package; without it the suite is skipped.
--
-- Known conversion failures ('ConvError', non-fixed points) never fail the
-- suite; they are tallied and listed in @<corpus>/REPORT.md@ (or
-- @$PANFORM_REPORT@). The suite exits
-- non-zero only when a reader or writer throws (an exception is a bug no
-- warning can excuse); conversions slower than 'limitSeconds' are reported.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM, forM, unless)
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST (Form, formItems)
import Panform.Conv
import Panform.Formats
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.IO (hFlush, stdout)
import System.Timeout (timeout)
import Text.Printf (printf)


-- | Seconds allowed for one read or write before it counts as a hang.
limitSeconds :: Int
limitSeconds = 60

-- | A hang is reported, not failed: a slow conversion is a performance
-- bug, not a crash.
data Outcome a = Done a | Failed T.Text | Crashed String | TimedOut

-- | Run a pure conversion, forcing its result, catching exceptions and hangs.
guarded :: (a -> Int) -> Conv a -> IO (Outcome a)
guarded size c = do
  r <- try (timeout (limitSeconds * 1000000) (evaluate (forceRes (runConv c))))
  pure $ case r of
    Left e -> Crashed (firstLine (show (e :: SomeException)))
    Right Nothing -> TimedOut
    Right (Just (Left (ConvError m))) -> Failed m
    Right (Just (Right (a, _))) -> Done a
  where
    forceRes x@(Right (a, _)) = size a `seq` x
    forceRes x = x

formSize :: Form -> Int
formSize = length . show

data Failure = Failure
  { fTarget :: String
  , fFile :: FilePath
  , fKind :: String
  , fDetail :: String
  }

main :: IO ()
main = do
  corpusDir <- fromMaybe "../panform-corpus" <$> lookupEnv "PANFORM_CORPUS"
  present <- doesDirectoryExist corpusDir
  unless present $ do
    putStrLn ("corpus not found, skipping (looked in " <> corpusDir <> "; set PANFORM_CORPUS)")
    exitSuccess
  files <- corpusFiles corpusDir
  let targets = [f | f <- formats, isJust (fmtReader f), isJust (fmtWriter f)]
  results <- forM files $ \(fmt, path) -> do
    printf "%-14s %s\n" (fmtName fmt) path >> hFlush stdout
    bytes <- BL.readFile (corpusDir </> path)
    r <- maybe (pure (Failed "no reader")) (\rd -> guarded formSize (rd bytes)) (fmtReader fmt)
    case r of
      Done s -> do
        fs <- concat <$> mapM (roundTrip path s) targets
        own <- ownFormat path fmt bytes
        pure (fmt, path, Right (null (formItems s)), own ++ fs)
      Failed m -> pure (fmt, path, Left ("read error" :: String, T.unpack m), [])
      Crashed m -> pure (fmt, path, Left ("READER CRASH", m), [])
      TimedOut -> pure (fmt, path, Left ("reader timeout", timeoutMsg), [])
  let readRows =
        M.toList $
          M.fromListWith
            addR
            [(fmtName fmt, tally rr) | (fmt, _, rr, _) <- results]
      -- (read ok, of which empty, read error, crash, files)
      tally :: Either (String, String) Bool -> (Int, Int, Int, Int, Int)
      tally = \case
        Right empty -> (1, fromEnum empty, 0, 0, 1)
        Left ("READER CRASH", _) -> (0, 0, 0, 1, 1)
        Left _ -> (0, 0, 1, 0, 1)
      addR (a, b, c, d, e) (a', b', c', d', e') = (a + a', b + b', c + c', d + d', e + e')
      readOk = length [() | (_, _, Right _, _) <- results]
      emptyReads = [(fmtName fmt, p) | (fmt, p, Right True, _) <- results]
      readFailures = [(fmtName fmt, p, k, m) | (fmt, p, Left (k, m), _) <- results]
      allF = concat [fs | (_, _, _, fs) <- results]
      byTarget k t = length [() | f <- allF, fTarget f == t, fKind f == k]
      targetRows =
        [ (n, readOk, byTarget "write error" n, byTarget "cannot read own output" n, byTarget "not a fixed point" n, slow, crashes, byTarget "silent change in own format" n)
        | t <- targets
        , let n = fmtName t
              slow = length [() | f <- allF, fTarget f == n, "timeout" `isPrefixOf` fKind f]
              crashes = length [() | f <- allF, fTarget f == n, "CRASH" `isPrefixOf` fKind f]
        ]
      readerTable =
        [ "| source format | files | read ok | read ok but empty | read error | crash |"
        , "|---|---:|---:|---:|---:|---:|"
        ]
          ++ [printf "| %s | %d | %d | %d | %d | %d |" n tot ok em err cr | (n, (ok, em, err, cr, tot)) <- readRows]
      targetTable =
        [ "| target format | inputs | write error | cannot read own output | not a fixed point | timeout | crash | clean | own format: silent change |"
        , "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
        ]
          ++ [ printf "| %s | %d | %d | %d | %d | %d | %d | %d | %d |" n i w c p sl cr (i - w - c - p - sl - cr) own
             | (n, i, w, c, p, sl, cr, own) <- targetRows
             ]
      failureList =
        [ "## Reader failures"
        , ""
        ]
          ++ [printf "- `%s` (%s): %s: %s" p n k (oneLine m) | (n, p, k, m) <- readFailures]
          ++ [printf "- `%s` (%s): read ok but no items" p n | (n, p) <- emptyReads]
          ++ concat
            [ ["", "## Target " <> fmtName t, ""]
                ++ [printf "- `%s`: %s: %s" (fFile f) (fKind f) (oneLine (fDetail f)) | f <- allF, fTarget f == fmtName t]
            | t <- targets
            , any ((== fmtName t) . fTarget) allF
            ]
      report =
        unlines $
          [ "# Corpus report"
          , ""
          , "Generated by `cabal test corpus` (test/Corpus.hs). Do not edit by hand."
          , "Provenance for every file is in `SOURCES.tsv`."
          , ""
          , "## Readers"
          , ""
          ]
            ++ readerTable
            ++ ["", "## Round trips (write -> read -> write) per target format", ""]
            ++ targetTable
            ++ [""]
            ++ failureList
  -- PANFORM_REPORT lets parallel runs write separate reports.
  reportPath <- fromMaybe (corpusDir </> "REPORT.md") <$> lookupEnv "PANFORM_REPORT"
  writeFile reportPath report
  putStrLn ""
  mapM_ putStrLn readerTable
  putStrLn ""
  mapM_ putStrLn targetTable
  let crashes =
        [(p, m) | (_, p, k, m) <- readFailures, k == "READER CRASH"]
          ++ [(fFile f, fTarget f <> ": " <> fDetail f) | f <- allF, "CRASH" `isPrefixOf` fKind f]
  unless (null crashes) $ do
    putStrLn "\nCrashes:"
    mapM_ (\(p, m) -> putStrLn ("  " <> p <> ": " <> m)) crashes
    exitFailure

-- | Read in its own format and written back, a file reads back to the same
-- form (extension attributes included), unless something warned.
ownFormat :: FilePath -> FormatSpec -> BL.ByteString -> IO [Failure]
ownFormat path f bytes = case (fmtReader f, fmtWriter f) of
  (Just rd, Just wr) -> do
    r <- try (evaluate (check rd wr)) :: IO (Either SomeException Bool)
    pure [Failure (fmtName f) path "silent change in own format" "reads back different, with no warning" | Right True <- [r]]
  _ -> pure []
  where
    check rd wr = case runConv (rd bytes) of
      Right (s0, _) -> case runConv (wr s0) of
        Right (out, []) -> case runConv (rd out) of
          Right (s1, []) -> s1 /= s0
          _ -> False
        _ -> False
      _ -> False

-- | Write with a target, read that back, write again: the two writes must
-- match byte for byte and the reader must accept its own writer's output.
roundTrip :: FilePath -> Form -> FormatSpec -> IO [Failure]
roundTrip path s t = case (fmtReader t, fmtWriter t) of
  (Just rd, Just wr) -> do
    o1 <- guarded (fromIntegral . BL.length) (wr s)
    case o1 of
      TimedOut -> pure [Failure name path "timeout in writer" timeoutMsg]
      Crashed m -> pure [Failure name path "CRASH in writer" m]
      Failed m -> pure [Failure name path "write error" (T.unpack m)]
      Done out1 -> do
        r <- guarded formSize (rd out1)
        case r of
          TimedOut -> pure [Failure name path "timeout reading own output" timeoutMsg]
          Crashed m -> pure [Failure name path "CRASH reading own output" m]
          Failed m -> pure [Failure name path "cannot read own output" (T.unpack m)]
          Done s' -> do
            o2 <- guarded (fromIntegral . BL.length) (wr s')
            pure $ case o2 of
              TimedOut -> [Failure name path "timeout in second write" timeoutMsg]
              Crashed m -> [Failure name path "CRASH in second write" m]
              Failed m -> [Failure name path "not a fixed point" ("second write failed: " <> T.unpack m)]
              Done out2
                | out1 == out2 -> []
                | otherwise -> [Failure name path "not a fixed point" (firstDiff out1 out2)]
  _ -> pure []
  where
    name = fmtName t

-- | The first line where two outputs differ, shown from just before the
-- first differing character.
firstDiff :: BL.ByteString -> BL.ByteString -> String
firstDiff a b =
  case [(n, x, y) | (n, x, y) <- zip3 [1 :: Int ..] (pad la) (pad lb), x /= y] of
    ((n, x, y) : _) ->
      let k = length (takeWhile id (zipWith (==) (T.unpack x) (T.unpack y))) - 30
          clip = T.unpack . T.take 100 . T.drop (max 0 k)
       in printf "line %d: %s  =>  %s" n (clip x) (clip y)
    [] -> "outputs differ"
  where
    la = T.lines (sanitize a)
    lb = T.lines (sanitize b)
    len = max (length la) (length lb)
    pad ls = take len (ls ++ repeat "<eof>")

-- | Decode for display: UTF-8 (lenient), control characters made visible.
sanitize :: BL.ByteString -> T.Text
sanitize = T.map (\c -> if c /= '\n' && (c < ' ' || c == '\DEL') then '?' else c) . TE.decodeUtf8Lenient . BL.toStrict

timeoutMsg :: String
timeoutMsg = "no result after " <> show limitSeconds <> "s"

-- | Every file with its format, as a path relative to the corpus root.
corpusFiles :: FilePath -> IO [(FormatSpec, FilePath)]
corpusFiles corpusDir = do
  dirs <- sort <$> (listDirectory corpusDir >>= filterM (doesDirectoryExist . (corpusDir </>)))
  concat <$> forM dirs (\d -> case lookupFormat d of
    Nothing -> putStrLn ("skipping " <> d <> ": no format of that name") >> pure []
    Just fmt -> do
      names <- sort <$> listDirectory (corpusDir </> d)
      paths <- filterM (doesFileExist . (corpusDir </>)) [d </> n | n <- names, not ("." `isPrefixOf` takeFileName n)]
      pure [(fmt, p) | p <- paths])

firstLine :: String -> String
firstLine = take 300 . takeWhile (/= '\n')

oneLine :: String -> String
oneLine = take 300 . map (\c -> if c < ' ' || c == '\DEL' then ' ' else c)
