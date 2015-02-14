{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.JSON
  ( module Data.Aeson
  , module Data.Aeson.Types
  , object
  , record
  , (.+)
  , (.+?)
  , (.++)
  , (.!)
  , (.!?)
  , Query
  , jsonQuery
  ) where

import Data.Aeson hiding (object)
import Data.Aeson.Types hiding (object)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Data.Vector as V
import Network.HTTP.Types (Query)
import Network.URI (URI)

object :: [Pair] -> Object
object = HM.fromList

record :: ToJSON k => k -> [Pair] -> Object
record k = object . (("id" .= k) :)

infixl 4 .+, .+?, .++
(.+) :: Object -> Pair -> Object
o .+ (k, v) = HM.insert k v o

(.+?) :: Object -> Maybe Pair -> Object
o .+? Nothing = o
o .+? Just p = o .+ p

(.++) :: Object -> [Pair] -> Object
(.++) = foldl' (.+)

(.!) :: FromJSON a => Array -> Int -> Parser a
a .! i = maybe (fail $ "index " ++ show i ++ " out of range") parseJSON $ a V.!? i

(.!?) :: FromJSON a => Array -> Int -> Parser (Maybe a)
a .!? i = Trav.mapM parseJSON $ a V.!? i

instance ToJSON BS.ByteString where
  toJSON = String . TE.decodeUtf8

instance ToJSON URI where
  toJSON = toJSON . show

jsonQuery :: (Monad m) => Object -> (BS.ByteString -> Maybe BS.ByteString -> m (Maybe Value)) -> Query -> m Object
jsonQuery j _ [] = return j
jsonQuery j f ((k,v):q) = do
  o <- f k v
  jsonQuery (j .+? fmap ((,) (TE.decodeLatin1 k)) o) f q
