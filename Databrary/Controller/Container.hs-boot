module Databrary.Controller.Container where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Action

postContainer :: AppRoute (API, Id Slot)
createContainer :: AppRoute (API, Id Volume)
