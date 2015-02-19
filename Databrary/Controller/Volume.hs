{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( withVolume
  , viewVolume
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.Action
import Databrary.DB
import Databrary.Model.Enum
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Citation
import Databrary.Model.Funding
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Controller.Permission

withVolume :: Permission -> Id Volume -> (Volume -> AuthAction) -> AppAction
withVolume p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupVolume i

volumeJSONField :: (DBM m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("party" JSON..= partyJSON (volumeAccessParty va)))
    <$> volumeVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField vol "citation" _ =
  Just . maybe JSON.Null JSON.toJSON <$> volumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toJSON <$> volumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.toJSON <$> volumeFunding vol
volumeJSONField vol "containers" _ =
  Just . JSON.toJSON . map containerJSON <$> volumeContainers vol
volumeJSONField vol "records" _ =
  Just . JSON.toJSON . map recordJSON <$> volumeRecords vol
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: (DBM m, MonadHasIdentity c m) => Volume -> JSON.Query -> m JSON.Object
volumeJSONQuery vol = JSON.jsonQuery (volumeJSON vol) (volumeJSONField vol)

displayVolume :: Maybe JSON.Query -> Volume -> AuthAction
displayVolume (Just q) vol = okResponse [] =<< volumeJSONQuery vol q
displayVolume Nothing vol = okResponse [] $ volumeName vol -- TODO

viewVolume :: Bool -> Id Volume -> AppRAction
viewVolume api i = action GET (apiRoute api $ toRoute i) $ do
  q <- peeks Wai.queryString
  withVolume PermissionPUBLIC i $
    displayVolume (q <? api)
