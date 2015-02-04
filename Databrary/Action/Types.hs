module Databrary.Action.Types
  ( Action
  , withAction
  , ActionData(..)
  , ActionM
  , WaiAction
  , runWai
  ) where

import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT)
import Network.HTTP.Types (ResponseHeaders, Status)
import qualified Network.Wai as Wai

import Control.Has (Has, HasM, peeks)
import Databrary.Action.Request
import Databrary.Action.Response

type Action q = ReaderT q IO Response

withAction :: (q -> q') -> Action q' -> Action q
withAction = withReaderT

class Has Request q => ActionData q where
  returnResponse :: (HasM Request q m, ResponseData r) => Status -> ResponseHeaders -> r -> m Response
  returnResponse s h r = peeks (returnResponse s h r :: Request -> Response)

type ActionM q m = (HasM Request q m, ActionData q)


type WaiAction = Action Request

runWai :: WaiAction -> Wai.Application
runWai wai request send =
  send =<< catch
    (runReaderT wai request)
    (return . resultResponse)

instance ActionData Request where
  returnResponse s h = return . response s h
