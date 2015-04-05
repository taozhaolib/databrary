module Databrary.Controller.Asset where

import Databrary.Model.Id.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Action

data AssetTarget
  = AssetTargetVolume Volume
  | AssetTargetSlot Slot
  | AssetTargetAsset AssetSlot

postAsset :: API -> Id Asset -> AppRAction
createAsset :: API -> Id Volume -> AppRAction
createSlotAsset :: API -> Id Slot -> AppRAction
