module Databrary.Controller.AssetSegment where

import Databrary.Model.Id.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Action

serveAssetSegment :: Bool -> AssetSegment -> AuthAction
downloadAssetSegment :: AppRoute (Id Slot, Id Asset)
