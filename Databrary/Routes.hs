{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Monad (msum)
import Control.Monad.Writer (tell)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (ok200)

import qualified Databrary.Route as R
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Party

testParty :: Id Party -> BAction
testParty i = do
  p <- getParty i
  respond $ maybe "not found" partyName p
  return ok200

routes :: R.RouteM SomeAction
routes = msum 
  [ "party" >> routeId >>= action . testParty
  ]
