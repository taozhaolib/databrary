module Databrary.Action.Types
  ( Action
  , ActionData(..)
  , ActionM
  , WaiAction
  , runWai
  ) where

import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, runReaderT)
import Network.HTTP.Types (ResponseHeaders, Status)
import qualified Network.Wai as Wai

import Control.Has (Has, MonadHas, peeks)
import Databrary.Web.Request
import Databrary.Action.Response

type Action q = ReaderT q IO Response

class Has Request q => ActionData q where
  returnResponse :: (MonadHas Request q m, ResponseData r) => Status -> ResponseHeaders -> r -> m Response
  returnResponse s h r = peeks (returnResponse s h r :: Request -> Response)

type ActionM q m = (MonadHas Request q m, ActionData q)


type WaiAction = Action Request

runWai :: WaiAction -> Wai.Application
runWai wai request send =
  send =<< catch
    (runReaderT wai request)
    (return . resultResponse)

instance ActionData Request where
  returnResponse s h = return . response s h
