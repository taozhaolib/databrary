{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Web.Constants
  ( constantsJSON
  , constantsJS
  , generateConstantsJSON
  , generateConstantsJS
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import Data.Version (showVersion)
import System.IO (withFile, IOMode(WriteMode))

import qualified Paths_databrary as Databrary
import Databrary.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service
import Databrary.Store
import Databrary.Model.Enum
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Metric
import Databrary.Model.RecordCategory
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Web.Files

constantsJSON :: JSON.Value
constantsJSON = JSON.Object $ JSON.object
  [ "permission" JSON..= enumValues PermissionPUBLIC
  , "release" JSON..= enumValues ReleasePUBLIC
  , "metric" JSON..= JSON.recordMap (map metricJSON allMetrics)
  , "category" JSON..= JSON.recordMap (map recordCategoryJSON allRecordCategories)
  , "format" JSON..= JSON.recordMap (map formatJSON allFormats)
  , "party" JSON..= JSON.object
    [ "nobody" JSON..= partyJSON nobodyParty
    , "root" JSON..= partyJSON rootParty
    ]
  , "version" JSON..= showVersion Databrary.version
#ifdef DEVEL
  , "devel" JSON..= True
#endif
  -- TODO: mode? url? version?
  ]
  where
  enumValues :: forall a . DBEnum a => a -> [String]
  enumValues _ = map show $ enumFromTo minBound (maxBound :: a)

constantsJSONB :: BSB.Builder
constantsJSONB = JSON.encodeToByteStringBuilder constantsJSON

constantsJS :: BSB.Builder
constantsJS = BSB.string7 "app.constant('constantData'," <> constantsJSONB <> BSB.string7 ");"

regenerateConstants :: (MonadHasService c m, MonadIO m) => BSB.Builder -> RawFilePath -> m Bool
regenerateConstants b f = do
  st <- peeks serviceStartTime
  liftIO $ webRegenerate st f $ \wf ->
    withFile (unRawFilePath wf) WriteMode $ \h ->
      BSB.hPutBuilder h b

generateConstantsJSON :: (MonadHasService c m, MonadIO m) => RawFilePath -> m Bool
generateConstantsJSON = regenerateConstants constantsJSONB

generateConstantsJS :: (MonadHasService c m, MonadIO m) => RawFilePath -> m Bool
generateConstantsJS = regenerateConstants constantsJS
