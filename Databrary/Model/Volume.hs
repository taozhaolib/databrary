{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Volume 
  ( module Databrary.Model.Volume.Types
  , lookupVolume
  , changeVolume
  , addVolume
  , volumeJSON
  ) where

import Data.Maybe (catMaybes)

import Control.Applicative.Ops
import Control.Has (peek, view)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL

useTPG

lookupVolume :: (DBM m, MonadHasIdentity c m) => Id Volume -> m (Maybe Volume)
lookupVolume vi = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectVolume 'ident) "$WHERE volume.id = ${vi}")

volumeJSON :: Volume -> JSON.Object
volumeJSON Volume{..} = JSON.record volumeId $ catMaybes
  [ Just $ "name" JSON..= volumeName
  , "alias" JSON..= volumeAlias <? (volumePermission >= PermissionREAD)
  , Just $ "body" JSON..= volumeBody
  , Just $ "creation" JSON..= volumeCreation
  , Just $ "permission" JSON..= volumePermission
  ]

changeVolume :: MonadAudit c m => Volume -> m ()
changeVolume v = do
  ident <- getAuditIdentity
  dbExecute1 $(updateVolume 'ident 'v)

addVolume :: MonadAudit c m => Volume -> m Volume
addVolume bv = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap ($ PermissionADMIN) $(insertVolume 'ident 'bv)

