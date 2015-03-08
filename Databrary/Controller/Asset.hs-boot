module Databrary.Controller.Asset where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

createAsset :: API -> Id Volume -> AppRAction
