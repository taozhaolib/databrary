{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeAccess.SQL
  ( selectVolumeVolumeAccess
  ) where

import Databrary.Model.SQL
import Databrary.Model.Party.SQL
import Databrary.Model.VolumeAccess.Types

volumeAccessRow :: Selector -- ^ @'Party' -> 'Volume' -> 'VolumeAccess'@
volumeAccessRow = selectColumns 'VolumeAccess "volume_access" ["individual", "children"]

selectVolumeVolumeAccess :: Selector -- ^ 'Volume' -> 'VolumeAccess'
selectVolumeVolumeAccess = selectJoin '($)
  [ volumeAccessRow
  , joinOn "volume_access.party = party.id" selectParty
  ]
