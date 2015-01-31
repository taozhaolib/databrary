module Databrary.App
  ( application
  ) where

import Network.Wai (Application)

import Databrary.Resource
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Action.Route
import Databrary.Routes

application :: Resource -> Application
application rc = runWai $ runApp rc $ runRoute routes
