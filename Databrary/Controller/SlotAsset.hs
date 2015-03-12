{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.SlotAsset
  ( downloadSlotAsset
  ) where

import Data.Maybe (fromJust)

import Control.Has (view)
import Databrary.Action.Route
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Container.Types
import Databrary.Model.Asset
import Databrary.Model.SlotAsset
import Databrary.Controller.Permission
import Databrary.Store.Asset
import Databrary.Web.File

getSlotAsset :: Permission -> Id Container -> Id Asset -> AuthActionM SlotAsset
getSlotAsset p ci ai =
  checkPermission p =<< maybeAction =<< lookupSlotAsset ai

downloadSlotAsset :: Id Container -> Id Asset -> AppRAction
downloadSlotAsset ci ai = action GET (ci, ai) $ withAuth $ do
  sa <- getSlotAsset PermissionREAD ci ai
  let a = view sa
  store <- maybeAction =<< getAssetFile a
  auditSlotAssetDownload True sa
  serveFile store (view a) (fromJust $ assetSHA1 a)
