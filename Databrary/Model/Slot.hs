{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Slot
  ( module Databrary.Model.Slot.Types
  , lookupSlot
  , slotJSON
  ) where

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Segment
import Databrary.Model.Container
import Databrary.Model.Slot.Types

lookupSlot :: (MonadDB m, MonadHasIdentity c m) => Id Slot -> m (Maybe Slot)
lookupSlot (Id (SlotId c s)) =
  fmap (`Slot` s) <$> lookupContainer c

slotJSON :: Slot -> JSON.Object
slotJSON Slot{..} = containerJSON slotContainer JSON..+?
  ( segmentFull slotSegment ?!> ("segment" JSON..= slotSegment)
  )
