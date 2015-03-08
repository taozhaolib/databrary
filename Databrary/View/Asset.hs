{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Asset
  ( htmlAssetForm
  ) where

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Asset

htmlAssetForm :: Volume -> AuthRequest -> FormHtml
htmlAssetForm vol req = htmlForm "Create asset" (createAsset HTML $ volumeId vol) req $ do
  field "name" $ inputText (Nothing :: Maybe String)
  field "classification" $ inputEnum (Just ClassificationRESTRICTED)
  -- TODO
