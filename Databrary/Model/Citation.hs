{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation
  ( module Databrary.Model.Citation.Types
  , volumeCitation
  , volumeLinks
  ) where

import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Volume.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Citation.SQL

volumeCitation :: (DBM m) => Volume -> m (Maybe Citation)
volumeCitation vol =
  dbQuery1 $ fmap ($ Just (volumeName vol)) $(selectQuery selectVolumeCitation "$WHERE volume_citation.volume = ${volumeId vol}")

volumeLinks :: (DBM m) => Volume -> m [Citation]
volumeLinks vol =
  dbQuery $(selectQuery selectVolumeLink "$WHERE volume_link.volume = ${volumeId vol}")
