-- | The AST itself as JSON: lossless, and the interchange point for
-- external filters (@panform -t native | jq ... | panform -f native@).
module Panform.Format.Native (readNative, writeNative) where

import Data.Aeson (eitherDecode, toJSON)
import qualified Data.ByteString.Lazy as BL
import Panform.AST
import Panform.Conv
import Panform.Json (encodePretty)

readNative :: BL.ByteString -> Conv Form
readNative bs = eitherDecode bs `orFail` "native JSON"

writeNative :: Form -> Conv BL.ByteString
writeNative = pure . encodePretty . toJSON
