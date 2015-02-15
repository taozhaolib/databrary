{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Volume.SQL
  ( selectVolume
  ) where

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Language.Haskell.TH as TH

import Databrary.Model.Time.Types
import Databrary.Model.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types

defaulting :: a -> (a -> b) -> Maybe a -> b
defaulting d f = f . fromMaybe d

setCreation :: (Timestamp -> a) -> Maybe Timestamp -> a
setCreation = defaulting (posixSecondsToUTCTime 1357900000)

setPermission :: (Permission -> a) -> Maybe Permission -> a
setPermission = defaulting PermissionNONE

volumeRow :: Selector -- ^ @'Permission' -> 'Volume'@
volumeRow = addSelects 'setCreation
  (selectColumns 'Volume "volume" ["id", "name", "alias", "body"])
  ["volume_creation(volume.id)"]

selectVolume :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Volume'@
selectVolume i = selectJoin 'setPermission
  [ volumeRow
  , joinOn "volume_permission.permission >= 'PUBLIC'::permission"
    $ selector ("LATERAL (VALUES (CASE WHEN ${identitySuperuser " ++ is ++ "} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${see " ++ is ++ " :: Id Party}) END)) AS volume_permission (permission)") "volume_permission.permission"
  ]
  where is = nameRef i
