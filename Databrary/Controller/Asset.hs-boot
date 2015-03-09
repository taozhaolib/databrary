module Databrary.Controller.Asset where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Asset.Types
import Databrary.Action

postAsset :: API -> Id Asset -> AppRAction
createAsset :: API -> Id Volume -> AppRAction
