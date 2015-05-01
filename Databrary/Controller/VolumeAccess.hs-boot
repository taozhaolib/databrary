module Databrary.Controller.VolumeAccess where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Action

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

viewVolumeAccess :: AppRoute (Id Volume, VolumeAccessTarget)
postVolumeAccess :: AppRoute (API, Id Volume, VolumeAccessTarget)
