{-# LANGUAGE GADTs #-}
module Databrary.HTTP.Path.Types
  ( Path
  , PathDynamic(..)
  , pathDynamicAs
  , PathElement(..)
  , PathElements
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Text
import Data.Typeable (Typeable)
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Model.Offset
import Databrary.Model.Segment

type Path = [T.Text]

class Typeable a => PathDynamic a where
  pathDynamic :: T.Text -> Maybe a
  dynamicPath :: a -> T.Text

pathDynamicAs :: PathDynamic a => Proxy a -> T.Text -> Maybe a
pathDynamicAs _ = pathDynamic

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
  PathElementDynamic :: PathDynamic a => a -> PathElement
  PathElementAny :: Path -> PathElement

type PathElements = [PathElement]
