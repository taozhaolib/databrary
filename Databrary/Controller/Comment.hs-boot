module Databrary.Controller.Comment where

import Databrary.Model.Id.Types
import Databrary.Model.Slot.Types
import Databrary.Action

postComment :: API -> Id Slot -> AppRAction
