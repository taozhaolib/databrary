{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
module Databrary.HTTP.Route.Path
  ( Path
  , renderPath
  , PathDynamic(..)
  , PathElement(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Hashable (Hashable(..))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Text
import Data.Typeable (Typeable, typeRep)
import Network.HTTP.Types.URI (encodePathSegments)
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Model.Offset
import Databrary.Model.Segment

type Path = [T.Text]

renderPath :: Path -> BSB.Builder
renderPath [] = BSB.char7 '/'
renderPath p = encodePathSegments p

class Typeable a => PathDynamic a where
  pathDynamic :: T.Text -> Maybe a
  dynamicPath :: a -> T.Text

instance PathDynamic T.Text where
  pathDynamic = Just
  dynamicPath = id

instance PathDynamic BS.ByteString where
  pathDynamic = Just . TE.encodeUtf8
  dynamicPath = TE.decodeUtf8

readText :: Text.Reader a -> T.Text -> Maybe a
readText = (either (const Nothing) (\(a, t) -> T.null t ?> a) .)

instance PathDynamic Integer where
  pathDynamic = readText (Text.signed Text.decimal)
  dynamicPath = T.pack . show

instance PathDynamic Int where
  pathDynamic = readText (Text.signed Text.decimal)
  dynamicPath = T.pack . show

instance PathDynamic Int64 where
  pathDynamic = readText (Text.signed Text.decimal)
  dynamicPath = T.pack . show

instance PathDynamic Int32 where
  pathDynamic = readText (Text.signed Text.decimal)
  dynamicPath = T.pack . show

instance PathDynamic Int16 where
  pathDynamic = readText (Text.signed Text.decimal)
  dynamicPath = T.pack . show

instance PathDynamic Offset where
  pathDynamic = readMaybe . T.unpack
  dynamicPath = T.pack . show . offsetMillis

instance PathDynamic Segment where
  pathDynamic = readMaybe . T.unpack
  dynamicPath s = T.pack $ showSegmentWith (shows . offsetMillis) s ""


data PathElement where
  PathElementFixed :: !T.Text -> PathElement
  PathElementDynamic :: PathDynamic a => !(Proxy a) -> PathElement

instance Eq PathElement where
  PathElementFixed a == PathElementFixed b = a == b
  PathElementDynamic a == PathElementDynamic b = typeRep a == typeRep b
  _ == _ = False

instance Ord PathElement where
  PathElementFixed a   `compare` PathElementFixed b   = a `compare` b
  PathElementFixed _   `compare` PathElementDynamic _ = LT
  PathElementDynamic _ `compare` PathElementFixed _   = GT
  PathElementDynamic a `compare` PathElementDynamic b = typeRep a `compare` typeRep b

instance Hashable PathElement where
  hashWithSalt s (PathElementFixed t)   = hashWithSalt s (0 :: Int) `hashWithSalt` t
  hashWithSalt s (PathElementDynamic d) = hashWithSalt s (1 :: Int) `hashWithSalt` typeRep d

pathCheckElement :: PathElement -> T.Text -> Bool
pathCheckElement (PathElementFixed e) = (e ==)
pathCheckElement (PathElementDynamic e) = ok e . pathDynamic where
  ok :: Proxy a -> Maybe a -> Bool
  ok _ = isJust

pathCheckElements :: [PathElement] -> Path -> Bool
pathCheckElements [] [] = True
pathCheckElements (e:el) (p:pl) = pathCheckElement e p && pathCheckElements el pl
pathCheckElements _ _ = False
