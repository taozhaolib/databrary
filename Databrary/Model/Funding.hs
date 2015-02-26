{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Funding
  ( module Databrary.Model.Funding.Types
  , lookupFunder
  , lookupVolumeFunding
  ) where

import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Funding.Types
import Databrary.Model.Funding.SQL

useTPG

lookupFunder :: DBM m => Id Funder -> m (Maybe Funder)
lookupFunder fi =
  dbQuery1 $(selectQuery selectFunder "$WHERE funder.fundref_id = ${fi}")

lookupVolumeFunding :: (DBM m) => Volume -> m [Funding]
lookupVolumeFunding vol =
  dbQuery $(selectQuery selectVolumeFunding "$WHERE volume_funding.volume = ${volumeId vol}")
