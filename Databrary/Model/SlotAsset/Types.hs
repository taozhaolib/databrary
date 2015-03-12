{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SlotAsset.Types
  ( SlotAsset(..)
  , MonadHasSlotAsset
  ) where

import qualified Language.Haskell.TH as TH

import Control.Has (makeHasFor, Has(..))
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
  view SlotAsset{ slotAssetExcerpt = Just c } = c
  view SlotAsset{ slotAsset = a } = view a

instance Has Permission SlotAsset where
  view sa = dataPermission (view sa :: Asset) (view sa) (view sa)
