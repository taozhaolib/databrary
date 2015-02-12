{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeAccess.Types
  ( VolumeAccess(..)
  , MonadHasVolumeAccess
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types

data VolumeAccess = VolumeAccess
  { volumeAccessIndividual, volumeAccessChildren :: Permission
  , volumeAccessParty :: Party
  , volumeAccessVolume :: Volume
  }

makeHasRec ''VolumeAccess ['volumeAccessVolume, 'volumeAccessParty]
