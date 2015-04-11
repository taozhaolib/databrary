module Databrary.Model.AssetSegment.Types
  ( AssetSegment(..)
  , assetFullSegment
  ) where

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

data AssetSegment = AssetSegment
  { segmentAsset :: AssetSlot
  , assetSegment :: Segment
  , assetSegmentExcerpt :: Maybe Classification
  }

assetFullSegment :: AssetSlot -> Maybe AssetSegment
assetFullSegment a@(AssetSlot _ (Just s) e) = Just $ AssetSegment a (slotSegment s) e
assetFullSegment _ = Nothing

instance Has AssetSlot AssetSegment where
  view = segmentAsset
instance Has Asset AssetSegment where
  view = view . segmentAsset
instance Has (Id Asset) AssetSegment where
  view = view . segmentAsset
instance Has Volume AssetSegment where
  view = view . segmentAsset
instance Has (Id Volume) AssetSegment where
  view = view . segmentAsset

instance Has Slot AssetSegment where
  view AssetSegment{ segmentAsset = AssetSlot{ assetSlot = Just s }, assetSegment = seg } = s{ slotSegment = seg }
  view _ = error "unlinked AssetSegment"
instance Has Container AssetSegment where
  view = slotContainer . view
instance Has (Id Container) AssetSegment where
  view = containerId . slotContainer . view
instance Has Segment AssetSegment where
  view AssetSegment{ segmentAsset = AssetSlot{ assetSlot = Just s }, assetSegment = seg } = seg `segmentIntersect` slotSegment s
  view _ = emptySegment
instance Has (Maybe Consent) AssetSegment where
  view = view . (view :: AssetSegment -> Slot)

instance Has Classification AssetSegment where
  view AssetSegment{ assetSegmentExcerpt = Just c } = c
  view AssetSegment{ segmentAsset = a } = view a

instance Has Permission AssetSegment where
  view e = dataPermission (view e) (view e) (view e)
