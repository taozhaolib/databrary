module Databrary.Controller.Party where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

viewParty :: API -> PartyTarget -> AppRAction
postParty :: API -> PartyTarget -> AppRAction
createParty :: API -> AppRAction
queryParties :: API -> AppRAction
