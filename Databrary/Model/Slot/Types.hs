{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Slot.Types
  ( Slot(..)
  , MonadHasSlot
  ) where

import qualified Data.Text as T

import Control.Has (makeHasRec)
import Databrary.Time
import Databrary.Model.Kind
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types

data Slot = Slot
  { slotSegment :: Segment
  , slotContainer :: Container
  }

makeHasRec ''Slot ['slotContainer]
