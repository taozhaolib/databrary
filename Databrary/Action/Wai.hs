module Databrary.Action.Wai
  ( WaiM
  , WaiAction
  , runWai
  ) where

import Control.Exception (catch)
import qualified Network.Wai as Wai

import Databrary.Action.Types
import Databrary.Action.Response

type WaiM r = ActionM Wai.Request r
type WaiAction r = Action Wai.Request r

runWai :: Response r => WaiAction r -> Wai.Application
runWai wai request send =
  send =<< catch
    (runAction wai request)
    (return . resultResponse)
