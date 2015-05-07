module Databrary.Controller.VolumeAccess where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action
import Databrary.Controller.Paths

viewVolumeAccess :: AppRoute (Id Volume, VolumeAccessTarget)
postVolumeAccess :: AppRoute (API, (Id Volume, VolumeAccessTarget))
