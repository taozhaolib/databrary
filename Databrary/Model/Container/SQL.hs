{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Container.SQL
  ( selectVolumeContainer
  , selectContainer
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Volume.SQL
import Databrary.Model.Container.Types

containerRow :: Selector -- ^ @Maybe 'Consent' -> 'Permission' -> 'Container'@
containerRow = selectColumns 'Container "container" ["id", "top", "name", "date"]

selectVolumeContainer :: Selector -- ^ @'Volume' -> 'Container'@
selectVolumeContainer = selectJoin '($)
  [ containerRow
  , maybeJoinOn "container.id = slot_consent.container AND slot_consent.segment = '(,)'"
    $ selector "slot_consent" "consent"
  ]

selectContainer :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Container'@
selectContainer ident = selectJoin '($)
  [ selectVolumeContainer
  , joinOn "container.volume = volume.id" $ selectVolume ident
  ]
