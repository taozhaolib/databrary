{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , MonadHasAppRequest
  , AppAction
  , AppActionM
  , withApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)

import Control.Has (MonadHas, makeHasRec)
import Databrary.Web.HTTP
import Databrary.Resource
import Databrary.Model.Time.Types
import Databrary.Action.Types
import Databrary.Web.Request
import Databrary.Action.Response

data AppRequest = AppRequest
  { appResource :: !Resource
  , appTimestamp :: !Timestamp
  , appRequest :: !Request
  }

makeHasRec ''AppRequest ['appResource, 'appTimestamp, 'appRequest]

type AppAction = Action AppRequest
type AppActionM q m = (MonadHas AppRequest q m, ActionData q)

withApp :: Resource -> AppAction -> WaiAction
withApp rc act = do
  ts <- liftIO getCurrentTime
  withAction (AppRequest rc ts) act

instance ActionData AppRequest where
  returnResponse s h r = do
    ts <- asks appTimestamp
    return $ response s ((hDate, formatHTTPTimestamp ts) : h) r
