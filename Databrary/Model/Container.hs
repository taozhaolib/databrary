{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Container
  ( module Databrary.Model.Container.Types
  , blankContainer
  , lookupContainer
  , lookupVolumeContainer
  , lookupVolumeContainers
  , addContainer
  , changeContainer
  , removeContainer
  , containerJSON
  ) where

import Data.Maybe (catMaybes)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Databrary.Ops
import Databrary.Has (view, peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL

useTPG

blankContainer :: Volume -> Container
blankContainer vol = Container
  { containerId = error "blankContainer"
  , containerTop = False
  , containerName = Nothing
  , containerDate = Nothing
  , containerConsent = Nothing
  , containerVolume = vol
  }

lookupContainer :: (MonadDB m, MonadHasIdentity c m) => Id Container -> m (Maybe Container)
lookupContainer ci = do
  ident <- peek
  dbQuery1 $(selectQuery (selectContainer 'ident) "$WHERE container.id = ${ci}")

lookupVolumeContainer :: MonadDB m => Volume -> Id Container -> m (Maybe Container)
lookupVolumeContainer vol ci =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.id = ${ci} AND container.volume = ${volumeId vol}")

lookupVolumeContainers :: MonadDB m => Volume -> m [Container]
lookupVolumeContainers vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.volume = ${volumeId vol}")

addContainer :: MonadAudit c m => Container -> m Container
addContainer bc = do
  ident <- getAuditIdentity
  dbQuery1' $(insertContainer 'ident 'bc)

changeContainer :: MonadAudit c m => Container -> m ()
changeContainer c = do
  ident <- getAuditIdentity
  dbExecute1' $(updateContainer 'ident 'c)

removeContainer :: MonadAudit c m => Container -> m ()
removeContainer c = do
  ident <- getAuditIdentity
  dbExecute1' $(deleteContainer 'ident 'c)

formatContainerDate :: Container -> Maybe String
formatContainerDate c = formatTime defaultTimeLocale fmt <$> containerDate c where
  fmt
    | dataPermission (view c) ClassificationRESTRICTED (view c) >= PermissionREAD = "%Y-%m-%d"
    | otherwise = "%Y-XX-XX"

containerJSON :: Container -> JSON.Object
containerJSON c@Container{..} = JSON.record containerId $ catMaybes
  [ "top" JSON..= containerTop <? containerTop
  , ("name" JSON..=) <$> containerName
  , ("date" JSON..=) <$> formatContainerDate c
  , ("consent" JSON..=) <$> containerConsent
  ]

