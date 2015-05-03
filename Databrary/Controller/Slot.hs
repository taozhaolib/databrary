{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Slot
  ( getSlot
  , viewSlot
  ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.Excerpt
import Databrary.Model.RecordSlot
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Angular

getSlot :: Permission -> Id Slot -> AuthActionM Slot
getSlot p i =
  checkPermission p =<< maybeAction =<< lookupSlot i

slotJSONField :: (MonadDB m, MonadHasIdentity c m) => Slot -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
slotJSONField o "assets" _ =
  Just . JSON.toJSON . map assetSlotJSON <$> lookupSlotAssets o
slotJSONField o "records" _ =
  Just . JSON.toJSON . map recordSlotJSON <$> lookupSlotRecords o
slotJSONField o "tags" n = do
  ident <- peek
  tc <- lookupSlotTagCoverage ident o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map tagCoverageJSON tc
slotJSONField o "comments" n = do
  c <- lookupSlotComments o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map commentJSON c
slotJSONField o "excerpts" _ =
  Just . JSON.toJSON . map excerptJSON <$> lookupSlotExcerpts o
slotJSONField _ _ _ = return Nothing

slotJSONQuery :: (MonadDB m, MonadHasIdentity c m) => Slot -> JSON.Query -> m JSON.Object
slotJSONQuery o = JSON.jsonQuery (slotJSON o) (slotJSONField o)

viewSlot :: AppRoute (API, Id Slot)
viewSlot = action GET (pathAPI </> pathSlotId) $ \(api, i) -> withAuth $ do
  when (api == HTML) angular
  c <- getSlot PermissionPUBLIC i
  case api of
    JSON -> okResponse [] =<< slotJSONQuery c =<< peeks Wai.queryString
    HTML -> okResponse [] $ BSC.pack $ show $ containerId $ slotContainer c -- TODO

