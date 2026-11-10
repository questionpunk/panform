-- | Small helpers for reading loosely-typed JSON formats without partial
-- functions.
module Panform.Json
  ( key
  , keyText
  , keyTextOr
  , keyArr
  , keyBool
  , keyNum
  , asText
  , obj
  , (.=?)
  , encodePretty
  ) where

import Data.Aeson (Key, Value (..), object)
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import Data.Scientific (base10Exponent, floatingOrInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

key :: Text -> Value -> Maybe Value
key k (Object o) = case KM.lookup (K.fromText k) o of
  Just Null -> Nothing
  v -> v
key _ _ = Nothing

-- | A value as text; numbers and booleans are rendered, so a choice code
-- written as @1@ or @"1"@ reads the same.
asText :: Value -> Maybe Text
asText = \case
  String t -> Just t
  Number n
    -- writing out an integer with a huge exponent (1e999999999) would
    -- build a number with that many digits; it keeps its own spelling
    | base10Exponent n > 400 -> Just (T.pack (show n))
    | otherwise -> Just $ either (T.pack . show @Double) (T.pack . show @Integer) (floatingOrInteger n)
  Bool b -> Just (if b then "true" else "false")
  _ -> Nothing

keyText :: Text -> Value -> Maybe Text
keyText k v = key k v >>= asText

keyTextOr :: Text -> Text -> Value -> Text
keyTextOr d k v = maybe d id (keyText k v)

keyArr :: Text -> Value -> [Value]
keyArr k v = case key k v of
  Just (Array a) -> V.toList a
  Just (Object o) -> KM.elems o
  _ -> []

keyBool :: Text -> Value -> Maybe Bool
keyBool k v = case key k v of
  Just (Bool b) -> Just b
  Just (String s) -> Just (T.toLower s `elem` ["true", "1", "yes"])
  Just (Number n) -> Just (n /= 0)
  _ -> Nothing

keyNum :: Text -> Value -> Maybe Double
keyNum k v = case key k v of
  Just (Number n) -> Just (toRealFloat n)
  Just (String s) -> case reads (T.unpack s) of
    [(d, "")] -> Just d
    _ -> Nothing
  _ -> Nothing

-- | An object from optional fields; 'Nothing' fields are omitted.
obj :: [Maybe (Key, Value)] -> Value
obj = object . catMaybes

(.=?) :: Key -> Value -> Maybe (Key, Value)
k .=? v = Just (k, v)

infixr 8 .=?

encodePretty :: Value -> BL.ByteString
encodePretty = P.encodePretty' P.defConfig {P.confIndent = P.Spaces 2, P.confCompare = compare}
