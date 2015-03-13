{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Asset
  ( htmlAssetForm
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Asset

htmlAssetForm :: Either Volume AssetSlot -> AuthRequest -> FormHtml
htmlAssetForm targ req = htmlForm
  (maybe "Create asset" (("Edit asset " <>) . fromMaybe "" . assetName) asset)
  (either (createAsset HTML . volumeId) (postAsset HTML . assetId . slotAsset) targ) req
  $ do
  field "name" $ inputText (assetName =<< asset)
  field "classification" $ inputEnum $ Just $ maybe ClassificationRESTRICTED assetClassification asset
  -- TODO
  where
  asset = either (const Nothing) (Just . slotAsset) targ
