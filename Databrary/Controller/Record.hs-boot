module Databrary.Controller.Record where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

createRecord :: API -> Id Volume -> AppRAction
