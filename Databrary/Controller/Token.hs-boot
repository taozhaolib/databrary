module Databrary.Controller.Token where

import Databrary.Model.Id.Types
import Databrary.Model.Token.Types
import Databrary.Action (AppRAction)

viewLoginToken :: Bool -> Id LoginToken -> AppRAction
postPasswordToken :: Bool -> Id LoginToken -> AppRAction
