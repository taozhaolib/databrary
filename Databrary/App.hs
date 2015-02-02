module Databrary.App
  ( application
  ) where

import Network.Wai (Application)

import Databrary.Resource
import Databrary.Action
import Databrary.Routes

application :: Resource -> Application
application = runAppRoute routes
