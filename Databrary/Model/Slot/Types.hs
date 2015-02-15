{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Slot.Types
  ( Slot(..)
  , MonadHasSlot
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Time.Types
import Databrary.Model.Container.Types

data Slot = Slot
  { slotContainer :: Container
  , slotSegment :: Segment
  }

makeHasRec ''Slot ['slotContainer, 'slotSegment]
