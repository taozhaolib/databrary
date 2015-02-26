{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation
  ( module Databrary.Model.Citation.Types
  , volumeCitation
  , setVolumeCitation
  , volumeLinks
  , setVolumeLinks
  ) where

import Control.Applicative ((<$>))

import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Citation.SQL

volumeCitation :: (DBM m) => Volume -> m (Maybe Citation)
volumeCitation vol =
  dbQuery1 $ fmap ($ Just (volumeName vol)) $(selectQuery selectVolumeCitation "$WHERE volume_citation.volume = ${volumeId vol}")

volumeLinks :: (DBM m) => Volume -> m [Citation]
volumeLinks vol =
  dbQuery $(selectQuery selectVolumeLink "$WHERE volume_link.volume = ${volumeId vol}")

setVolumeCitation :: (AuditM c m) => Volume -> Maybe Citation -> m Bool
setVolumeCitation vol citem = do
  ident <- getAuditIdentity
  (0 <) <$> maybe
    (dbExecute $(deleteVolumeCitation 'ident 'vol))
    (\cite -> updateOrInsert
      $(updateVolumeCitation 'ident 'vol 'cite)
      $(insertVolumeCitation 'ident 'vol 'cite))
    citem

setVolumeLinks :: (AuditM c m) => Volume -> [Citation] -> m ()
setVolumeLinks vol links = do
  ident <- getAuditIdentity
  dbTransaction $ do
    dbExecute $(deleteVolumeLink 'ident 'vol)
    mapM_ (\link -> dbExecute $(insertVolumeLink 'ident 'vol 'link)) links
