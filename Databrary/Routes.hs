{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Monad (msum)
import Network.HTTP.Types (methodGet, methodPost)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Controller.Login
import Databrary.Controller.Party

routes :: R.RouteM AppRAction
routes = do
  api <- True <$ R.fixed "api" <|> return False
  msum 
    [ "login" >> R.method >>= R.switch
      [ (methodGet,  return $ viewLogin api)
      , (methodPost, return $ postLogin api)
      ]
    , R.route >>= \party -> R.on GET >> return (getParty api party)
    ]
