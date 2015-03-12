module Databrary.Controller.Angular where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Network.Wai as Wai

import Databrary.Action

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, Blaze.Builder)
angularConstants :: AppRAction
