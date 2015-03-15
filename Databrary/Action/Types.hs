module Databrary.Action.Types
  ( ActionM
  , Action
  , ActionData(..)
  , MonadAction
  , WaiAction
  , runWai
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Network.HTTP.Types (ResponseHeaders, Status)
import qualified Network.Wai as Wai

import Databrary.Has (Has, MonadHas, peeks)
import Databrary.Web.Request
import Databrary.Action.Response

type ActionM q a = ReaderT q IO a
type Action q = ActionM q Response

class Has Request q => ActionData q where
  returnResponse :: (MonadHas Request q m, ResponseData r) => Status -> ResponseHeaders -> r -> m Response
  returnResponse s h r = peeks (returnResponse s h r :: Request -> Response)

type MonadAction q m = (MonadHas Request q m, ActionData q)


type WaiAction = Action Request

runWai :: WaiAction -> Wai.Application
runWai wai request send =
  send =<< runResult (runReaderT wai request)

instance ActionData Request where
  returnResponse s h = return . response s h
