module Databrary.Controller.Party where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action (AppRAction)

viewParty :: Bool -> Maybe (Id Party) -> AppRAction
postParty :: Bool -> Maybe (Id Party) -> AppRAction
createParty :: Bool -> AppRAction
searchParty :: Bool -> AppRAction
