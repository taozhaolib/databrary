{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.AssetSegment
  ( getAssetSegment
  , viewAssetSegment
  , serveAssetSegment
  , downloadAssetSegment
  , thumbAssetSegment
  ) where

import Control.Monad (when, mfilter)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing, isJust)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Typed.Range as Range
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Offset
import Databrary.Model.Volume
import Databrary.Model.Slot
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Store.Asset
import Databrary.Store.Filename
import Databrary.HTTP.File
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Parser
import Databrary.Media.AV
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Angular
import Databrary.Controller.Permission
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.Asset
import Databrary.Controller.Format

getAssetSegment :: Permission -> Maybe (Id Volume) -> Id Slot -> Id Asset -> AuthActionM AssetSegment
getAssetSegment p mv s a =
  checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupSlotAssetSegment s a

assetSegmentJSONField :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
assetSegmentJSONField _ _ _ = return Nothing

assetSegmentJSONQuery :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> JSON.Query -> m JSON.Object
assetSegmentJSONQuery vol = JSON.jsonQuery (assetSegmentJSON vol) (assetSegmentJSONField vol)

assetSegmentDownloadName :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> m [T.Text]
assetSegmentDownloadName a = do
  v <- volumeDownloadName (view a)
  s <- slotDownloadName (view a)
  return $ v ++ s ++ assetDownloadName (view a)

viewAssetSegment :: AppRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
viewAssetSegment = action GET (pathAPI </>>> pathMaybe pathId </>> pathSlotId </> pathId) $ \(api, vi, si, ai) -> withAuth $ do
  when (api == HTML) angular
  as <- getAssetSegment PermissionPUBLIC vi si ai
  case api of
    JSON -> okResponse [] =<< assetSegmentJSONQuery as =<< peeks Wai.queryString
    HTML -> okResponse [] $ T.pack $ show $ assetId $ slotAsset $ segmentAsset as -- TODO

serveAssetSegment :: Bool -> AssetSegment -> AuthAction
serveAssetSegment dl as = do
  _ <- checkDataPermission as
  store <- maybeAction =<< getAssetFile a
  szs <- peeks $ lookupQueryParameters "size"
  let sz = case szs of
        [Just n] -> fst <$> BSC.readInt n
        _ -> Nothing
  when dl $ auditAssetSegmentDownload True as
  dlname <- dl ?$> makeFilename <$> assetSegmentDownloadName as
  (hd, part) <- fileResponse store (view as) dlname etag
  av <- peek
  let samp p = do
        Just s <- liftIO $ avFrame av store p sz Nothing Nothing
        okResponse hd s
  if full
    then
      if isJust sz && afmt == imageFormat
        then samp Nothing
        else okResponse hd (store, part)
    else do
      maybe
        (fail "unimplemented")
        (\(Offset p) -> samp (Just p))
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

downloadAssetSegment :: AppRoute (Id Slot, Id Asset)
downloadAssetSegment = action GET (pathSlotId </> pathId </< "download") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionPUBLIC Nothing si ai
  inline <- peeks $ lookupQueryParameters "inline"
  serveAssetSegment (null inline) as

thumbAssetSegment :: AppRoute (Id Slot, Id Asset)
thumbAssetSegment = action GET (pathSlotId </> pathId </< "thumb") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionPUBLIC Nothing si ai
  q <- peeks Wai.queryString
  let as' = assetSegmentInterp 0.25 as
  if formatIsImage (view as')
    then do
      redirectRouteResponse [] downloadAssetSegment (slotId $ view as', assetId $ view as') q
    else
      redirectRouteResponse [] formatIcon (view as) q
