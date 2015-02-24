module Databrary.Controller.Token where

import Databrary.Model.Id.Types
import Databrary.Model.Token.Types
import Databrary.Action

viewLoginToken :: API -> Id LoginToken -> AppRAction
postPasswordToken :: API -> Id LoginToken -> AppRAction
