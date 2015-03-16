module Databrary.Controller.Container where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Action

postContainer :: API -> Id Slot -> AppRAction
createContainer :: API -> Id Volume -> AppRAction
