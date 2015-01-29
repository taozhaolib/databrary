{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , AppM
  , AppAction
  , AppBAction
  , runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)

import Control.Has
import Databrary.Web.HTTP
import Databrary.Resource
import Databrary.Types.Time
import Databrary.Action.Types
import Databrary.Action.Wai
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

type AppM r = ActionM AppRequest r
type AppAction r = Action AppRequest r
type AppBAction = BAction AppRequest

runApp :: Response r => Resource -> AppAction r -> WaiAction r
runApp rc act = do
  ts <- liftIO getCurrentTime
  responseHeader hDate $ httpTimestamp ts
  withAction (AppRequest rc ts) act
