{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , MonadHasAppRequest
  , MonadAppAction 
  , AppActionM
  , AppAction
  , runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Control.Has (makeHasRec)
import Databrary.Web.HTTP
import Databrary.Resource
import Databrary.Model.Time.Types
import Databrary.Action.Types
import Databrary.Web.Request
import Databrary.Action.Response

data AppRequest = AppRequest
  { appResource :: !Resource
  , appResourceState :: !InternalState
  , appTimestamp :: !Timestamp
  , appRequest :: !Request
  }

makeHasRec ''AppRequest ['appResource, 'appResourceState, 'appTimestamp, 'appRequest]

type AppActionM a = ActionM AppRequest a
type AppAction = Action AppRequest

type MonadAppAction q m = (MonadHasAppRequest q m, ActionData q)

runApp :: Resource -> AppAction -> Wai.Application
runApp rc act req send = do
  ts <- liftIO getCurrentTime
  runResourceT $ withInternalState $ \is ->
    send =<< runResult (runReaderT act (AppRequest rc is ts req))

instance ActionData AppRequest where
  returnResponse s h r = do
    ts <- asks appTimestamp
    return $ response s ((hDate, formatHTTPTimestamp ts) : h) r
