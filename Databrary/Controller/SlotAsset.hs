{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.SlotAsset
  ( downloadSlotAsset
  ) where

import Data.Maybe (fromJust)

import Control.Has (view)
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Container.Types
import Databrary.Model.Asset
import Databrary.Model.SlotAsset
import Databrary.Controller.Permission
import Databrary.Store.Asset
import Databrary.Web.File

withSlotAsset :: Permission -> Id Container -> Id Asset -> (SlotAsset -> AuthAction) -> AppAction
withSlotAsset p ci ai f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupSlotAsset ci ai

downloadSlotAsset :: Id Container -> Id Asset -> AppRAction
downloadSlotAsset ci ai = action GET (toRoute ci ++ toRoute ai) $
  withSlotAsset PermissionREAD ci ai $ \sa -> do
    let a = view sa
    store <- maybeAction =<< getAssetFile a
    auditSlotAssetDownload True sa
    serveFile store (view a) (fromJust $ assetSHA1 a)
