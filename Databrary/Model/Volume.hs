{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Volume 
  ( module Databrary.Model.Volume.Types
  , coreVolume
  , lookupVolume
  , changeVolume
  , addVolume
  , VolumeFilter(..)
  , findVolumes
  , volumeJSON
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..), (<>))
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteral)
import Database.PostgreSQL.Typed.Types (pgQuote)

import Databrary.Ops
import Databrary.Has (peek, view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Volume.Boot

useTPG

coreVolume :: Volume
coreVolume = $(loadVolume (Id 0) PermissionSHARED)

lookupVolume :: (MonadDB m, MonadHasIdentity c m) => Id Volume -> m (Maybe Volume)
lookupVolume vi = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectVolume 'ident) "$WHERE volume.id = ${vi}")

changeVolume :: MonadAudit c m => Volume -> m ()
changeVolume v = do
  ident <- getAuditIdentity
  dbExecute1' $(updateVolume 'ident 'v)

addVolume :: MonadAudit c m => Volume -> m Volume
addVolume bv = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap ($ PermissionADMIN) $(insertVolume 'ident 'bv)

volumeJSON :: Volume -> JSON.Object
volumeJSON Volume{..} = JSON.record volumeId $ catMaybes
  [ Just $ "name" JSON..= volumeName
  , "alias" JSON..= volumeAlias <? (volumePermission >= PermissionREAD)
  , Just $ "body" JSON..= volumeBody
  , Just $ "creation" JSON..= volumeCreation
  , Just $ "permission" JSON..= volumePermission
  ]

data VolumeFilter = VolumeFilter
  { volumeFilterQuery :: Maybe String
  , volumeFilterParty :: Maybe (Id Party)
  }

instance Monoid VolumeFilter where
  mempty = VolumeFilter Nothing Nothing
  mappend (VolumeFilter q1 p1) (VolumeFilter q2 p2) =
    VolumeFilter (q1 <> q2) (p1 <|> p2)

volumeFilter :: VolumeFilter -> String
volumeFilter VolumeFilter{..} =
  withq volumeFilterParty (const " JOIN volume_access ON volume.id = volume_access.volume")
  ++ withq volumeFilterQuery (\n -> " JOIN volume_text_idx ON volume.id = volume_text_idx.volume, plainto_tsquery('english', " ++ pgQuote n ++ ") query")
  ++ " WHERE volume.id > 0 AND "
  ++ withq volumeFilterParty (\p -> " AND volume_access.party = " ++ pgSafeLiteral p ++ " volume_access.individual >= 'EDIT'")
  ++ withq volumeFilterQuery (const " AND ts @@ query")
  ++ " ORDER BY "
  ++ withq volumeFilterQuery (const "ts_rank(ts, query) DESC,")
  ++ withq volumeFilterParty (const "volume_access.individual DESC,")
  ++ "volume.id DESC"
  where
  withq v f = maybe "" f v

findVolumes :: (MonadHasIdentity c m, MonadDB m) => VolumeFilter -> Int -> Int -> m [Volume]
findVolumes pf limit offset = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectVolume 'ident) "")
    (++ volumeFilter pf ++ " LIMIT " ++ show limit ++ " OFFSET " ++ show offset)
