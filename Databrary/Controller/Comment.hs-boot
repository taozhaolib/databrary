module Databrary.Controller.Comment where

import Databrary.Model.Id.Types
import Databrary.Model.Slot.Types
import Databrary.Action

postComment :: AppRoute (API, Id Slot)
