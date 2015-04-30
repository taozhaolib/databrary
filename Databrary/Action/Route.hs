{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , RouteAction
  , actionURL
  , action
  , multipartAction
  , API(..)
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
import Databrary.HTTP.Route.Path
import Databrary.HTTP.Route.PathParser
import Databrary.HTTP.Route

type RouteAction a q = Route a (Action q)

actionURL :: RouteAction a q -> a -> Maybe Request -> BS.ByteString
actionURL Route{ routeMethod = g, routePath = p } a req
  | g == methodGet = BSL.toStrict $ BSB.toLazyByteString
    $ maybe id ((<>) . BSB.byteString . requestHost) req $ renderPath $ pathGenerate p a
  | otherwise = error $ "actionURL: " ++ BSC.unpack g

action :: StdMethod -> PathParser a -> (a -> Action q) -> RouteAction a q
action = Route . renderStdMethod

multipartAction :: RouteAction a q -> RouteAction a q
multipartAction = id -- { routeMultipart = True }

data API
  = HTML
  | JSON
  deriving (Eq)

pathAPI :: PathParser API
pathAPI = HTML =/= I.constant JSON I.<$> "api"
