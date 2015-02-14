{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Container
  ( module Databrary.Model.Container.Types
  , volumeContainers
  , containerJSON
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Control.Has (see)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Permission
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL

useTPG

volumeContainers :: DBM m => Volume -> m [Container]
volumeContainers vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectContainer "$WHERE container.volume = ${volumeId vol}")

formatContainerDate :: Container -> Maybe String
formatContainerDate c = formatTime defaultTimeLocale fmt <$> containerDate c where
  fmt
    | dataPermission c ClassificationRESTRICTED (see c) >= PermissionREAD = "%Y-%m-%d"
    | otherwise = "%Y-XX-XX"

containerJSON :: Container -> JSON.Object
containerJSON c@Container{..} = JSON.record containerId $ catMaybes
  [ "top" JSON..= containerTop <$ guard containerTop
  , ("name" JSON..=) <$> containerName
  , ("date" JSON..=) <$> formatContainerDate c
  ]

