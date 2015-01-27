{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Monad (msum)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Controller.Login

testParty :: Id Party -> AppRAction
testParty i = bAction GET (toRoute i) $ do
  p <- lookupParty i
  respond $ maybe "not found" partyName p
  okResult

routes :: R.RouteM AppRAction
routes = msum 
  [ R.on GET >> R.route >>= return . testParty
  , R.on POST >> "login" >> return postLogin
  ]
