{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Record
  ( htmlRecordForm
  ) where

import qualified Data.ByteString.Char8 as BSC

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.RecordCategory
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Record

htmlRecordForm :: Volume -> AuthRequest -> FormHtml
htmlRecordForm vol req = htmlForm "Create record" (createRecord HTML $ volumeId vol) req $ do
  field "category" $ inputSelect Nothing $ ("", "<record>") : map (\c -> (BSC.pack $ show $ recordCategoryId c, recordCategoryName c)) allRecordCategories
