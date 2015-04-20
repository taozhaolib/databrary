{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation
  ( module Databrary.Model.Citation.Types
  , lookupVolumeCitation
  , changeVolumeCitation
  , lookupVolumeLinks
  , changeVolumeLinks
  ) where

import Control.Applicative ((<$>))

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Citation.SQL

lookupVolumeCitation :: (MonadDB m) => Volume -> m (Maybe Citation)
lookupVolumeCitation vol =
  dbQuery1 $ fmap ($ Just (volumeName vol)) $(selectQuery selectVolumeCitation "$WHERE volume_citation.volume = ${volumeId vol}")

lookupVolumeLinks :: (MonadDB m) => Volume -> m [Citation]
lookupVolumeLinks vol =
  dbQuery $(selectQuery selectVolumeLink "$WHERE volume_link.volume = ${volumeId vol}")

changeVolumeCitation :: (MonadAudit c m) => Volume -> Maybe Citation -> m Bool
changeVolumeCitation vol citem = do
  ident <- getAuditIdentity
  (0 <) <$> maybe
    (dbExecute $(deleteVolumeCitation 'ident 'vol))
    (\cite -> fst <$> updateOrInsert
      $(updateVolumeCitation 'ident 'vol 'cite)
      $(insertVolumeCitation 'ident 'vol 'cite))
    citem

changeVolumeLinks :: (MonadAudit c m) => Volume -> [Citation] -> m ()
changeVolumeLinks vol links = do
  ident <- getAuditIdentity
  dbTransaction $ do
    _ <- dbExecute $(deleteVolumeLink 'ident 'vol)
    mapM_ (\link -> dbExecute $(insertVolumeLink 'ident 'vol 'link)) links
