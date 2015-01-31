{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Monad (msum, mfilter)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Action.Route
import Databrary.Controller.Login
import Databrary.Controller.Party

act :: RouteAction q -> R.RouteM (Action q)
act ra = do
  _ <- mfilter (actionMethod ra ==) R.method
  return $ routeAction ra

routes :: R.RouteM AppAction
routes = do
  api <- True <$ R.fixed "api" <|> return False
  msum 
    [ "login" >> act (viewLogin api) 
             <|> act (postLogin api)
    , R.route >>= \party -> act (getParty api party)
    ]
