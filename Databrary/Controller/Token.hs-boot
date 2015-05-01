module Databrary.Controller.Token where

import Databrary.Model.Id.Types
import Databrary.Model.Token.Types
import Databrary.Action

viewLoginToken :: AppRoute (API, Id LoginToken)
postPasswordToken :: AppRoute (API, Id LoginToken)
