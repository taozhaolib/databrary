{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Monad (msum, mfilter)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Controller.Login
import Databrary.Controller.Party
import Databrary.Controller.Volume
import Databrary.Controller.Record
import Databrary.Controller.Static

act :: RouteAction q -> R.RouteM (Action q)
act ra = do
  _ <- mfilter (actionMethod ra ==) R.method
  return $ routeAction ra

routes :: R.RouteM AppAction
routes = do
  api <- True <$ R.fixed "api" <|> return False
  msum 
    [ "login" >>              act (viewLogin api) 
                          <|> act (postLogin api)
    , R.route >>= \p ->       act (viewParty api p)
                          <|> act (postParty api p)
    , "party" >>              act (createParty api)
                          <|> act (searchParty api)
    , R.route >>= \v ->       act (viewVolume api v)
    , R.route >>= \r ->       act (viewRecord api r)
    , "public" >> R.route >>= act . staticPublicFile
    ]
