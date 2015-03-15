{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Slot.Types
  ( Slot(..)
  , MonadHasSlot
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Id
import Databrary.Model.Kind
import Databrary.Model.Segment
import Databrary.Model.Container.Types

type instance IdType Slot = (Int32, Segment)

data Slot = Slot
  { slotContainer :: Container
  , slotSegment :: Segment
  }

instance Kinded Slot where
  kindOf _ = "slot"

makeHasRec ''Slot ['slotContainer, 'slotSegment]
