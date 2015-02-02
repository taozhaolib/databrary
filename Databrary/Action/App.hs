{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , AppAction
  , runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)

import Control.Has (makeHasFor, peek)
import Databrary.Web.HTTP
import Databrary.Resource
import Databrary.Types.Time
import Databrary.Action.Types
import Databrary.Action.Request
import Databrary.Action.Response

data AppRequest = AppRequest
  { appResource :: !Resource
  , appTimestamp :: !Timestamp
  , appRequest :: !Request
  }

makeHasFor ''AppRequest
  [ ('appResource, [])
  , ('appTimestamp, [])
  , ('appRequest, [])
  ]

type AppAction = Action AppRequest

runApp :: Resource -> AppAction -> WaiAction
runApp rc act = do
  ts <- liftIO getCurrentTime
  withAction (AppRequest rc ts) act

instance ActionData AppRequest where
  returnResponse s h r = do
    ts <- peek
    return $ response s ((hDate, formatHTTPTimestamp ts) : h) r
