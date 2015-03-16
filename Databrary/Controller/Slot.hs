{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Slot
  ( getSlot
  , viewSlot
  ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.RecordSlot
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Angular

getSlot :: Permission -> Id Slot -> AuthActionM Slot
getSlot p i =
  checkPermission p =<< maybeAction =<< lookupSlot i

slotJSONField :: (DBM m, MonadHasIdentity c m) => Slot -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
slotJSONField o "assets" _ =
  Just . JSON.toJSON . map assetSlotJSON <$> lookupSlotAssets o
slotJSONField o "records" _ =
  Just . JSON.toJSON . map recordSlotJSON <$> lookupSlotRecords o
slotJSONField _ _ _ = return Nothing

slotJSONQuery :: (DBM m, MonadHasIdentity c m) => Slot -> JSON.Query -> m JSON.Object
slotJSONQuery o = JSON.jsonQuery (slotJSON o) (slotJSONField o)

viewSlot :: API -> Id Slot -> AppRAction
viewSlot api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  c <- getSlot PermissionPUBLIC i
  case api of
    JSON -> okResponse [] =<< slotJSONQuery c =<< peeks Wai.queryString
    HTML -> okResponse [] $ show $ containerId $ slotContainer c -- TODO

