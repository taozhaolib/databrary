module Databrary.Model.AssetSlot.Types
  ( AssetSlot(..)
  , assetNoSlot
  ) where

import Databrary.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Consent
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Slot.Types

-- | An entire asset in its assigned position.
data AssetSlot = AssetSlot
  { slotAsset :: Asset
  , assetSlot :: Maybe Slot
  }

assetNoSlot :: Asset -> AssetSlot
assetNoSlot a = AssetSlot a Nothing

instance Has Asset AssetSlot where
  view = slotAsset
instance Has (Id Asset) AssetSlot where
  view = view . slotAsset
instance Has Format AssetSlot where
  view = view . slotAsset
instance Has (Id Format) AssetSlot where
  view = view . slotAsset
instance Has Classification AssetSlot where
  view = view . slotAsset
instance Has Volume AssetSlot where
  view = view . slotAsset
instance Has (Id Volume) AssetSlot where
  view = view . slotAsset
instance Has Permission AssetSlot where
  view = view . slotAsset

instance Has (Maybe Slot) AssetSlot where
  view = assetSlot
instance Has (Maybe Container) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe (Id Container)) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe Segment) AssetSlot where
  view = fmap view . assetSlot
instance Has Segment AssetSlot where
  view = maybe emptySegment slotSegment . assetSlot
instance Has (Maybe Consent) AssetSlot where
  view = (view =<<) . assetSlot
