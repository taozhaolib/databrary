{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Monad (msum)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (status200)

import Databrary.Route
import Databrary.Action

routes :: RouteM (Action ByteString)
routes = msum 
  [ fixed "foo" >> return (return status200)
  , path >> return defaultAction
  ] 
