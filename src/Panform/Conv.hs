-- | The pure conversion monad: a computation that may fail with an error and
-- accumulates warnings about information it could not carry across.
module Panform.Conv
  ( Conv
  , Warning (..)
  , ConvError (..)
  , runConv
  , warn
  , failWith
  , orFail
  , warnFixedCodes
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST

-- | What was lost or approximated, and where.
data Warning = Warning
  { warnWhere :: Text
  , warnWhat :: Text
  }
  deriving (Eq, Show)

newtype ConvError = ConvError Text
  deriving (Eq, Show)

type Conv = WriterT [Warning] (Except ConvError)

runConv :: Conv a -> Either ConvError (a, [Warning])
runConv = runExcept . runWriterT

warn :: Text -> Text -> Conv ()
warn w m = tell [Warning w m]

failWith :: Text -> Conv a
failWith = throwError . ConvError

orFail :: Either String a -> Text -> Conv a
orFail (Right a) _ = pure a
orFail (Left e) ctx = failWith (ctx <> ": " <> T.pack e)

-- | Formats whose "other" (or exclusive "none") option is a built-in with a
-- fixed code: warn when a choice's own code will not survive.
warnFixedCodes :: Text -> Maybe Text -> Maybe Text -> [Choice] -> Conv ()
warnFixedCodes at otherCode noneCode cs =
  mapM_ note $
    [(c, code, "\"other\"") | Just code <- [otherCode], c <- cs, choiceOther c, choiceValue c /= code]
      ++ [(c, code, "exclusive") | Just code <- [noneCode], c <- cs, choiceExclusive c, not (choiceOther c), choiceValue c /= code]
  where
    note (c, code, what) = warn at (what <> " option code " <> choiceValue c <> " becomes " <> code)
