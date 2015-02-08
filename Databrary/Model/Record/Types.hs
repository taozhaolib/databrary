{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Record.Types
  ( Record(..)
  , MonadHasRecord
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.RecordCategory.Types

type instance IdType Record = Int32

data Record = Record
  { recordId :: Id Record
  , recordVolume :: Volume
  , recordCategory :: RecordCategory
  }

instance Kinded Record where
  kindOf _ = "record"

makeHasRec ''Record ['recordId, 'recordVolume, 'recordCategory]
