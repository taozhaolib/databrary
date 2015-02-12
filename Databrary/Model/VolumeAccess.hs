{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.VolumeAccess
  ( module Databrary.Model.VolumeAccess.Types
  , volumeVolumeAccess
  , volumeAccessJSON
  ) where

import Control.Applicative ((<$))
import Control.Monad (guard)
import Data.Maybe (catMaybes)

import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeAccess.Types
import Databrary.Model.VolumeAccess.SQL

volumeVolumeAccess :: DBM m => Volume -> Permission -> m [VolumeAccess]
volumeVolumeAccess vol perm =
  dbQuery $ fmap ($ vol) $ $(selectQuery selectVolumeVolumeAccess "$WHERE volume_access.volume = ${volumeId vol} AND volume_access.individual >= ${perm} ORDER BY individual DESC, children DESC")

volumeAccessJSON :: VolumeAccess -> JSON.Object
volumeAccessJSON VolumeAccess{..} = JSON.object $ catMaybes
  [ ("individual" JSON..= volumeAccessIndividual) <$ guard (volumeAccessIndividual >= PermissionNONE)
  , ("children"   JSON..= volumeAccessChildren)   <$ guard (volumeAccessChildren   >= PermissionNONE)
  ]
