module Databrary.Controller.AssetSegment
  ( getAssetSegment
  , viewAssetSegment
  ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Network.Wai as Wai

import Databrary.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.Controller.Permission

getAssetSegment :: Permission -> Id Slot -> Id Asset -> AuthActionM AssetSegment
getAssetSegment p s a =
  checkPermission p =<< maybeAction =<< lookupSlotAssetSegment s a

assetSegmentJSONField :: (DBM m, MonadHasIdentity c m) => AssetSegment -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
assetSegmentJSONField _ _ _ = return Nothing

assetSegmentJSONQuery :: (DBM m, MonadHasIdentity c m) => AssetSegment -> JSON.Query -> m JSON.Object
assetSegmentJSONQuery vol = JSON.jsonQuery (assetSegmentJSON vol) (assetSegmentJSONField vol)

viewAssetSegment :: API -> Id Slot -> Id Asset -> AppRAction
viewAssetSegment api si ai = action GET (api, si, ai) $ withAuth $ do
  when (api == HTML) angular
  as <- getAssetSegment PermissionPUBLIC si ai
  case api of
    JSON -> okResponse [] =<< assetSegmentJSONQuery as =<< peeks Wai.queryString
    HTML -> okResponse [] $ show $ assetId $ slotAsset $ segmentAsset as -- TODO
