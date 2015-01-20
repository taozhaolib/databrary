{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Monad (msum)
import Network.HTTP.Types (ok200)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Action.App
import Databrary.Action.Auth
import Databrary.Model.Id
import Databrary.Model.Party
-- import Databrary.Model.Volume

testParty :: Id Party -> AppBAction
testParty i = do
  p <- lookupParty i
  respond $ maybe "not found" partyName p
  return ok200

testVolume :: Id Party -> AuthBAction
testVolume i = do
  p <- lookupParty i -- getVolume i
  respond $ maybe "not found" partyName p
  return ok200

routes :: R.RouteM (SomeAction AppRequest)
routes = msum 
  [
    "party" >> routeId >>= action . testParty
  , "volume" >> routeId >>= action . appAuth . testVolume
  ]
