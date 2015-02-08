{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Volume 
  ( module Databrary.Model.Volume.Types
  , lookupVolume
  , volumeJSON
  ) where

import Control.Applicative ((<$))
import Control.Monad (guard)
import Data.Maybe (catMaybes)

import Control.Has (peek, see)
import Databrary.DB
import Databrary.Identity
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party.Types
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
  , "alias" JSON..= volumeAlias <$ guard (volumePermission >= PermissionREAD)
  , Just $ "body" JSON..= volumeBody
  , Just $ "creation" JSON..= volumeCreation
  , Just $ "permission" JSON..= volumePermission
  ]

