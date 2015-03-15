{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Slot.Types
  ( SlotId(..)
  , Slot(..)
  , slotId
  , MonadHasSlotId
  , MonadHasSlot
  ) where

import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Typed.Range as Range

import Control.Applicative.Ops
import Control.Has (makeHasRec)
import qualified Databrary.Web.Route as R
import Databrary.Model.Id
import Databrary.Model.Kind
import Databrary.Model.Segment
import Databrary.Model.Container.Types

data SlotId = SlotId
  { slotContainerId :: !(Id Container)
  , slotSegmentId :: !Segment
  }

instance R.Routable SlotId where
  route = SlotId <$> (Id <$> R.route) <*> (fromMaybe Range.full <$> R.route)
  toRoute (SlotId (Id c) s) = R.toRoute c ++ R.toRoute (Range.isFull s ?!> s :: Maybe Segment)

type instance IdType Slot = SlotId

data Slot = Slot
  { slotContainer :: !Container
  , slotSegment :: !Segment
  }

slotId :: Slot -> Id Slot
slotId (Slot c s) = Id $ SlotId (containerId c) s

instance Kinded Slot where
  kindOf _ = "slot"

makeHasRec ''SlotId ['slotContainerId, 'slotSegmentId]
makeHasRec ''Slot ['slotContainer, 'slotSegment]
