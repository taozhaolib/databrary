{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.VolumeAccess
  ( module Databrary.Model.VolumeAccess.Types
  , volumeVolumeAccess
  , partyVolumeAccess
  , volumeAccessJSON
  , changeVolumeAccess
  ) where

import Data.Maybe (catMaybes)

import Control.Applicative.Ops
import Control.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Audit
import Databrary.Model.VolumeAccess.Types
import Databrary.Model.VolumeAccess.SQL

volumeVolumeAccess :: (DBM m, MonadHasIdentity c m) => Volume -> Permission -> m [VolumeAccess]
volumeVolumeAccess vol perm = do
  ident <- peek
  dbQuery $(selectQuery (selectVolumeVolumeAccess 'vol 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, children DESC")

partyVolumeAccess :: (DBM m, MonadHasIdentity c m) => Party -> Permission -> m [VolumeAccess]
partyVolumeAccess p perm = do
  ident <- peek
  dbQuery $(selectQuery (selectPartyVolumeAccess 'p 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, children DESC")

volumeAccessJSON :: VolumeAccess -> JSON.Object
volumeAccessJSON VolumeAccess{..} = JSON.object $ catMaybes
  [ ("individual" JSON..= volumeAccessIndividual) <? (volumeAccessIndividual >= PermissionNONE)
  , ("children"   JSON..= volumeAccessChildren)   <? (volumeAccessChildren   >= PermissionNONE)
  ]

changeVolumeAccess :: (AuditM c m) => VolumeAccess -> m Bool
changeVolumeAccess va = do
  ident <- getAuditIdentity
  (0 <) <$> if volumeAccessIndividual va == PermissionNONE
    then dbExecute $(deleteVolumeAccess 'ident 'va)
    else updateOrInsert
      $(updateVolumeAccess 'ident 'va)
      $(insertVolumeAccess 'ident 'va)
