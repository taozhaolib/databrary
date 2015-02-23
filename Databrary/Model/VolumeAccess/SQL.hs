{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeAccess.SQL
  ( selectVolumeVolumeAccess
  , selectPartyVolumeAccess
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.VolumeAccess.Types

volumeAccessRow :: Selector -- ^ @'Party' -> 'Volume' -> 'VolumeAccess'@
volumeAccessRow = selectColumns 'VolumeAccess "volume_access" ["individual", "children"]

selectVolumeVolumeAccess :: TH.Name -- ^ 'Volume'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'VolumeAccess'
selectVolumeVolumeAccess vol ident = selectMap (`TH.AppE` TH.VarE vol) $ selectJoin '($)
  [ volumeAccessRow
  , joinOn ("volume_access.party = party.id AND volume_access.volume = ${volumeId " ++ nameRef vol ++ "}")
    $ selectParty ident
  ]

selectPartyVolumeAccess :: TH.Name -- ^ 'Party'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'VolumeAccess'
selectPartyVolumeAccess p ident = selectJoin '($)
  [ selectMap (`TH.AppE` TH.VarE p) $ volumeAccessRow
  , joinOn ("volume_access.volume = volume.id AND volume_access.party = ${partyId " ++ nameRef p ++ "}")
    $ selectVolume ident
  ]
