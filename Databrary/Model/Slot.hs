module Databrary.Model.Slot
  ( module Databrary.Model.Slot.Types
  , lookupSlot
  ) where

import Control.Applicative ((<$>))

import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Time
import Databrary.Model.Container
import Databrary.Model.Slot.Types

lookupSlot :: (DBM m, MonadHasIdentity c m) => Id Container -> Segment -> m (Maybe Slot)
lookupSlot ci seg =
  fmap (`Slot` seg) <$> lookupContainer ci
