module Databrary.Controller.Volume where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

viewVolume :: API -> Id Volume -> AppRAction
viewVolumeForm :: Id Volume -> AppRAction
postVolume :: API -> Id Volume -> AppRAction
createVolume :: API -> AppRAction
