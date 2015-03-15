module Databrary.Model.Excerpt.Types
  ( Excerpt(..)
  , assetExcerpt
  ) where

import Control.Monad (liftM2)

import Databrary.Has (Has(..))
import Databrary.Model.Permission
import Databrary.Model.Segment
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Consent.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types

data Excerpt = Excerpt
  { excerptAsset :: AssetSlot
  , excerptSegment :: Segment
  , excerptClassification :: Classification
  }

assetExcerpt :: AssetSlot -> Maybe Excerpt
assetExcerpt a = liftM2 (Excerpt a . slotSegment) (assetSlot a) (assetSlotExcerpt a)

instance Has AssetSlot Excerpt where
  view = excerptAsset
instance Has Asset Excerpt where
  view = view . excerptAsset
instance Has (Id Asset) Excerpt where
  view = view . excerptAsset
instance Has Volume Excerpt where
  view = view . excerptAsset
instance Has (Id Volume) Excerpt where
  view = view . excerptAsset

instance Has Slot Excerpt where
  view Excerpt{ excerptAsset = AssetSlot{ assetSlot = Just s }, excerptSegment = seg } = s{ slotSegment = seg }
  view _ = error "unlinked excerpt"
instance Has Container Excerpt where
  view = slotContainer . view
instance Has (Id Container) Excerpt where
  view = containerId . slotContainer . view
instance Has Segment Excerpt where
  view = excerptSegment
instance Has (Maybe Consent) Excerpt where
  view = view . (view :: Excerpt -> Slot)

instance Has Classification Excerpt where
  view = excerptClassification

instance Has Permission Excerpt where
  view e = dataPermission (view e) (view e) (view e)
