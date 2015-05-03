{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeOperators, QuasiQuotes #-}
module Databrary.Model.Slot
  ( module Databrary.Model.Slot.Types
  , lookupSlot
  , pathSlotId
  , slotJSON
  ) where

import Databrary.Ops
import qualified Databrary.Iso as I
import Databrary.Iso.TH
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.HTTP.Route.PathParser
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Segment
import Databrary.Model.Container
import Databrary.Model.Slot.Types

lookupSlot :: (MonadDB m, MonadHasIdentity c m) => Id Slot -> m (Maybe Slot)
lookupSlot (Id (SlotId c s)) =
  fmap (`Slot` s) <$> lookupContainer c

slotIdIso :: (Id Container, Segment) I.<-> SlotId
slotIdIso = [iso|(c, s) <-> SlotId c s|]

pathSlot :: PathParser SlotId
pathSlot = slotIdIso I.<$> (idIso I.<$> PathDynamic </> fullSegment =/= PathDynamic)

pathSlotId :: PathParser (Id Slot)
pathSlotId = pathIdWith pathSlot

slotJSON :: Slot -> JSON.Object
slotJSON Slot{..} = containerJSON slotContainer JSON..+?
  ( segmentFull slotSegment ?!> ("segment" JSON..= slotSegment)
  )
