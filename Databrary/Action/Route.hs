{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , actionURL
  , action
  , multipartAction
  , API(..)
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Network.HTTP.Types (methodGet, StdMethod(..), renderStdMethod)

import qualified Databrary.Iso as I
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.HTTP.Route.PathParser
import Databrary.HTTP.Route

actionURL :: Route a r -> a -> Maybe Request -> BS.ByteString
actionURL r@Route{ routeMethod = g } a req
  | g == methodGet = BSL.toStrict $ BSB.toLazyByteString
    $ maybe id ((<>) . BSB.byteString . requestHost) req $ renderRoute r a
  | otherwise = error $ "actionURL: " ++ BSC.unpack g

action :: StdMethod -> PathParser a -> (a -> Action q) -> Route a (Action q)
action m = Route (renderStdMethod m) False

multipartAction :: Route a q -> Route a q
multipartAction r = r{ routeMultipart = True }

data API
  = HTML
  | JSON
  deriving (Eq)

pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = HTML =/= I.constant JSON I.<$> pathJSON
