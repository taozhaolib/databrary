module Databrary.Action
  ( runAction
  , defaultAction
  ) where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (notFound404)

import Databrary.App
import Databrary.Wai

type ActionM r = WaiT r AppM
type Action r = ActionM r Status

runAction :: WaiResult r => Action r -> WaiApplication AppM
runAction = runWaiT

defaultAction :: Action ByteString
defaultAction = return notFound404
