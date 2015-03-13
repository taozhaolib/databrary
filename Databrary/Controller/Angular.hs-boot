module Databrary.Controller.Angular where

import qualified Data.ByteString.Builder as BSB
import qualified Network.Wai as Wai

import Databrary.Action

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, BSB.Builder)
angularConstants :: AppRAction
