module Databrary.Routes where

import qualified Data.ByteString.Builder as B

import Databrary.HTTP.Route
import Databrary.Action

routeMap :: RouteMap AppAction
jsRoutes :: B.Builder
