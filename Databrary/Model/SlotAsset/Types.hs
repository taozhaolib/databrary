{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SlotAsset.Types
  ( SlotAsset(..)
  , MonadHasSlotAsset
  ) where

import Control.Lens (set)
import qualified Language.Haskell.TH as TH

import Control.Has (makeHasFor, Has(..), view)
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Time.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Slot.Types

-- | An entire asset in its assigned position.
data SlotAsset = SlotAsset
  { slotAsset :: Asset
  , assetSlot :: Slot
  , slotAssetExcerpt :: Maybe Classification
  }

makeHasFor ''SlotAsset 
  [ ('slotAsset, 
    [ TH.ConT ''Id `TH.AppT` TH.ConT ''Asset
    , TH.ConT ''Format
    , TH.ConT ''Volume
    , TH.ConT ''Id `TH.AppT` TH.ConT ''Volume
    ])
  , ('assetSlot, 
    [ TH.ConT ''Container
    , TH.ConT ''Segment
    , TH.ConT ''Id `TH.AppT` TH.ConT ''Container
    , TH.ConT ''Maybe `TH.AppT` TH.ConT ''Consent
    ])
  ]

instance Has Classification SlotAsset where
  view f sa@SlotAsset{ slotAssetExcerpt = Just c } = fmap (\c' -> sa{ slotAssetExcerpt = Just c' }) $ f c
  view f sa@SlotAsset{ slotAsset = a } = fmap (\c -> sa{ slotAsset = set view c a }) $ f $ see a
  see SlotAsset{ slotAssetExcerpt = Just c } = c
  see SlotAsset{ slotAsset = a } = see a

instance Has Permission SlotAsset where
  see sa = dataPermission (see sa :: Asset) (see sa) (see sa)
