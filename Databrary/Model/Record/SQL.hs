{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Record.SQL
  ( recordRow
  , selectRecord
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.RecordCategory
import Databrary.Model.Record.Types

makeRecord :: Id Record -> Maybe (Id RecordCategory) -> Volume -> Record
makeRecord i c v = Record i v (fmap getRecordCategory' c)

recordRow :: Selector -- ^ @'Volume' -> 'Record'@
recordRow = selectColumns 'makeRecord "record" ["id", "category"]

selectRecord :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Record'@
selectRecord i = selectJoin '($)
  [ recordRow
  , joinOn "record.volume = volume.id" $ selectVolume i
  ]
