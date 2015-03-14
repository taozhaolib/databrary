module Databrary.Model.AssetSlot.Types
  ( AssetSlot(..)
  , assetNoSlot
  ) where

import Control.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Consent
import Databrary.Model.Time.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Slot.Types

-- | An entire asset in its assigned position.
data AssetSlot = AssetSlot
  { slotAsset :: Asset
  , assetSlot :: Maybe Slot
  , assetSlotExcerpt :: Maybe Classification
  }

assetNoSlot :: Asset -> AssetSlot
assetNoSlot a = AssetSlot a Nothing Nothing

instance Has Asset AssetSlot where
  view = slotAsset
instance Has (Id Asset) AssetSlot where
  view = view . slotAsset
instance Has Format AssetSlot where
  view = view . slotAsset
instance Has (Id Format) AssetSlot where
  view = view . slotAsset
instance Has Volume AssetSlot where
  view = view . slotAsset
instance Has (Id Volume) AssetSlot where
  view = view . slotAsset

instance Has (Maybe Slot) AssetSlot where
  view = assetSlot
instance Has (Maybe Container) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe (Id Container)) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe Segment) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe Consent) AssetSlot where
  view = (view =<<) . assetSlot

instance Has Classification AssetSlot where
  view AssetSlot{ assetSlotExcerpt = Just c } = c
  view AssetSlot{ slotAsset = a } = view a

instance Has Permission AssetSlot where
  view AssetSlot{ slotAsset = a, assetSlot = Just s } = dataPermission (view a) (view a) (view s)
  view AssetSlot{ slotAsset = a } = view a
