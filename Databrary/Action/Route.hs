{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , actionURL
  , action
  , multipartAction
  , API(..)
  , pathHTML
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Network.HTTP.Types (methodGet, StdMethod(..), renderStdMethod)

import qualified Databrary.Iso as I
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Route

actionURL :: Maybe Request -> Route r a -> a -> BSB.Builder
actionURL req r@Route{ routeMethod = g }
  | g == methodGet = routeURL req r
  | otherwise = error $ "actionURL: " ++ BSC.unpack g

action :: StdMethod -> PathParser a -> (a -> Action q) -> Route (Action q) a
action m = Route (renderStdMethod m) False

multipartAction :: Route q a -> Route q a
multipartAction r = r{ routeMultipart = True }

data API
  = HTML
  | JSON
  deriving (Eq)

pathHTML :: PathParser ()
pathHTML = PathEmpty

pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = HTML =/= I.constant JSON I.<$> pathJSON
