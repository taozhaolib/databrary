{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Volume 
  ( module Databrary.Model.Volume.Types
  , lookupVolume
  , volumeJSON
  , volumeJSONQuery
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Query)

import Control.Has (peek, see)
import Databrary.DB
import Databrary.Enum
import Databrary.Identity
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL

import Databrary.Model.VolumeAccess
import Databrary.Model.Party

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

volumeJSONField :: (DBM m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  i <- peek
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("party" JSON..= partyJSON (volumeAccessParty va) i))
    <$> volumeVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: (DBM m, MonadHasIdentity c m) => Volume -> Query -> m JSON.Object
volumeJSONQuery vol [] = return $ volumeJSON vol
volumeJSONQuery vol ((k,v):q) = do
  o <- volumeJSONField vol k v
  (JSON..+? fmap ((,) (TE.decodeLatin1 k)) o) <$> volumeJSONQuery vol q
