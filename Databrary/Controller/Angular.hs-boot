module Databrary.Controller.Angular where

import qualified Data.ByteString.Builder as BSB
import qualified Network.Wai as Wai

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, BSB.Builder)
