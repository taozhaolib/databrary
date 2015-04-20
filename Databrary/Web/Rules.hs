{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Rules
  ( generateWebFile
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Databrary.Service
import Databrary.Store
import Databrary.Web.Constants
import Databrary.Web.Templates

generateWebFile :: (MonadHasService c m, MonadIO m) => RawFilePath -> m (Maybe Bool)
generateWebFile f@"constants.json" = Just <$> generateConstantsJSON f
generateWebFile f@"constants.js" = Just <$> generateConstantsJS f
generateWebFile f@"templates.js" = Just <$> liftIO (generateTemplatesJS f)
generateWebFile _ = return Nothing

allWebFiles :: [RawFilePath]
allWebFiles = ["constants.json", "constants.js", "templates.js"]
