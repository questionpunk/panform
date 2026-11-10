module Main (main) where

import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Panform.Audit (auditMarkdown)
import Panform.Conv
import Panform.Formats
import Panform.Json (encodePretty)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

data Opts = Opts
  { optFrom :: Maybe String
  , optTo :: Maybe String
  , optOutput :: Maybe FilePath
  , optStrict :: Bool
  , optQuiet :: Bool
  , optList :: Bool
  , optAudit :: Bool
  , optCatalog :: Bool
  , optInput :: Maybe FilePath
  }

opts :: Parser Opts
opts =
  Opts
    <$> optional (strOption (long "from" <> short 'f' <> metavar "FORMAT" <> help "Input format (guessed from the file name if omitted)"))
    <*> optional (strOption (long "to" <> short 't' <> metavar "FORMAT" <> help "Output format (guessed from -o if omitted)"))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file (stdout if omitted)"))
    <*> switch (long "strict" <> help "Fail if anything would be lost or approximated")
    <*> switch (long "quiet" <> short 'q' <> help "Do not print the loss report")
    <*> switch (long "list-formats" <> help "List supported formats")
    <*> switch (long "audit" <> help "Print, per format, which survey features survive a round trip")
    <*> switch (long "catalog" <> help "Print JSON format and visual design capabilities for integrations")
    <*> optional (argument str (metavar "INPUT" <> help "Input file (stdin if omitted)"))

main :: IO ()
main = do
  -- Reports and warnings are UTF-8 whatever the locale: under LANG=C a
  -- warning naming a non-ASCII id would otherwise abort the conversion.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  o <- execParser (info (opts <**> helper) (fullDesc <> progDesc "Convert forms and surveys between formats" <> header "panform - pandoc for forms"))
  if optCatalog o
    then BL.putStr (encodePretty catalog)
    else if optAudit o
    then T.putStr (auditMarkdown [(fmtName f, fmtReader f, fmtWriter f) | f <- formats])
    else if optList o
    then forM_ formats $ \f ->
      putStrLn $
        padTo (maximum (map (length . fmtName) formats) + 1) (fmtName f)
          <> maybe "-" (const "r") (fmtReader f)
          <> maybe "-" (const "w") (fmtWriter f)
          <> "  "
          <> T.unpack (fmtDescription f)
          <> "  ("
          <> intercalate ", " (fmtExtensions f)
          <> ")"
    else run o
  where
    padTo n s = s <> replicate (n - length s) ' '

run :: Opts -> IO ()
run o = do
  from <- pick "input" (optFrom o) (optInput o)
  to <- pick "output" (optTo o) (optOutput o)
  input <- maybe BL.getContents BL.readFile (optInput o)
  case convert from to input of
    Left (ConvError e) -> die' e
    Right (out, ws) -> do
      unless (optQuiet o || null ws) $ do
        T.hPutStrLn stderr (T.pack (show (length ws)) <> " thing(s) did not convert exactly:")
        forM_ ws $ \(Warning w m) -> T.hPutStrLn stderr ("  [" <> w <> "] " <> m)
      when (optStrict o && not (null ws)) $ die' "--strict: refusing to write a lossy conversion"
      -- The output is lazy: build all of it before the output file is
      -- opened, so a failure can never leave a truncated file behind.
      built <- try (evaluate (BL.length out))
      case built of
        Left (e :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> die' ("internal error, nothing written: " <> T.pack (show e))
        Right _ -> maybe (BL.putStr out) (`BL.writeFile` out) (optOutput o)
  where
    pick what name path = case (name, path) of
      (Just n, _) -> maybe (die' ("unknown " <> T.pack what <> " format " <> T.pack n <> "; see --list-formats")) pure (lookupFormat n)
      (Nothing, Just p) -> maybe (die' ("cannot guess the " <> T.pack what <> " format from " <> T.pack p <> "; pass it explicitly")) pure (formatForPath p)
      (Nothing, Nothing) -> die' ("no " <> T.pack what <> " format given")
    die' m = T.hPutStrLn stderr ("panform: " <> m) >> exitFailure
