module Databrary.Controller.Party where

import Databrary.Action
import Databrary.Controller.Paths

viewParty :: AppRoute (API, PartyTarget)
postParty :: AppRoute (API, PartyTarget)
createParty :: AppRoute API
queryParties :: AppRoute API
