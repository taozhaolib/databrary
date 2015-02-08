{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Record
  ( module Databrary.Model.Record.Types
  , lookupRecord
  , lookupVolumeRecord
  , recordJSON
  ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import Control.Has (peek, see)
import Databrary.DB
import Databrary.Identity
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.RecordCategory
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL

useTPG

lookupRecord :: (MonadHasIdentity c m, DBM m) => Id Record -> m (Maybe Record)
lookupRecord i = do
  ident <- peek
  dbQuery1 $ $(selectQuery (selectRecord 'ident) "$WHERE record.id = ${i}")

lookupVolumeRecord :: DBM m => Volume -> Id Record -> m (Maybe Record)
lookupVolumeRecord v i =
  dbQuery1 $ fmap ($ v) $(selectQuery recordRow "$WHERE volume = ${volumeId v} AND record.id = ${i}")

recordJSON :: Record -> JSON.Object
recordJSON Record{..} = JSON.record recordId $ catMaybes
  [ Just $ "volume" JSON..= volumeId recordVolume
  , ("category" JSON..=) <$> fmap recordCategoryId recordCategory
  ]

