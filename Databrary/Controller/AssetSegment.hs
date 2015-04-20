{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.AssetSegment
  ( getAssetSegment
  , viewAssetSegment
  , serveAssetSegment
  , downloadAssetSegment
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing, isJust)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Typed.Range as Range
import qualified Network.Wai as Wai

import Databrary.Has (view, peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Offset
import Databrary.Model.Slot
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Store.Asset
import Databrary.HTTP.File
import Databrary.Media.AV
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.Controller.Permission

getAssetSegment :: Permission -> Id Slot -> Id Asset -> AuthActionM AssetSegment
getAssetSegment p s a =
  checkPermission p =<< maybeAction =<< lookupSlotAssetSegment s a

assetSegmentJSONField :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
assetSegmentJSONField _ _ _ = return Nothing

assetSegmentJSONQuery :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> JSON.Query -> m JSON.Object
assetSegmentJSONQuery vol = JSON.jsonQuery (assetSegmentJSON vol) (assetSegmentJSONField vol)

viewAssetSegment :: API -> Id Slot -> Id Asset -> AppRAction
viewAssetSegment api si ai = action GET (api, si, ai) $ withAuth $ do
  when (api == HTML) angular
  as <- getAssetSegment PermissionPUBLIC si ai
  case api of
    JSON -> okResponse [] =<< assetSegmentJSONQuery as =<< peeks Wai.queryString
    HTML -> okResponse [] $ T.pack $ show $ assetId $ slotAsset $ segmentAsset as -- TODO

serveAssetSegment :: AssetSegment -> AuthAction
serveAssetSegment as = do
  store <- maybeAction =<< getAssetFile a
  auditAssetSegmentDownload True as
  (hd, part) <- fileResponse store (view as) etag
  if full
    then okResponse hd (store, part)
    else do
      av <- peek
      maybe
        (fail "unimplemented")
        (\(Offset p) -> do
          Just samp <- liftIO $ avFrame av store (Just p) Nothing Nothing Nothing
          okResponse hd samp)
        point
  where
  a = slotAsset $ segmentAsset as
  afmt = assetFormat a
  clip = assetSegmentRange as
  point = Range.getPoint clip
  full = assetSegmentFull as || isNothing (assetDuration a) || isNothing (formatSample afmt)
  Just atag = assetSHA1 a
  etag = BSL.toStrict $ BSB.toLazyByteString $
    BSB.byteStringHex atag
    <> (if full then mempty else BSB.char7 ':' <> sb (Range.lowerBound clip)
    <> (if isJust point then mempty else BSB.char7 '-' <> sb (Range.upperBound clip)))
    where sb = maybe mempty (BSB.integerDec . offsetMillis) . Range.bound

downloadAssetSegment :: Id Slot -> Id Asset -> AppRAction
downloadAssetSegment si ai = action GET (si, ai, "download" :: T.Text) $ withAuth $ do
  as <- getAssetSegment PermissionREAD si ai
  serveAssetSegment as
