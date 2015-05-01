module Databrary.Controller.Party where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

viewParty :: AppRoute (API, PartyTarget)
postParty :: AppRoute (API, PartyTarget)
createParty :: AppRoute API
queryParties :: AppRoute API
