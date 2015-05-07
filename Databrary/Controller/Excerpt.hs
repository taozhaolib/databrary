{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Excerpt
  ( postExcerpt
  , deleteExcerpt
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), conflict409)

import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.AssetSegment

pathExcerpt :: PathParser (Id Slot, Id Asset)
pathExcerpt = pathJSON >/> pathSlotId </> pathId </< "excerpt"

postExcerpt :: AppRoute (Id Slot, Id Asset)
postExcerpt = action POST pathExcerpt $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionEDIT si ai
  c <- runForm Nothing $ 
    "release" .:> deformNonEmpty deform
  let e = Excerpt as c
  r <- changeExcerpt e
  guardAction r $
    returnResponse conflict409 [] ("The requested excerpt overlaps an existing excerpt." :: T.Text)
  okResponse [] $ assetSegmentJSON (if r then as{ assetExcerpt = Just e } else as)

deleteExcerpt :: AppRoute (Id Slot, Id Asset)
deleteExcerpt = action DELETE pathExcerpt $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionEDIT si ai
  r <- removeExcerpt as
  okResponse [] $ assetSegmentJSON (if r then as{ assetExcerpt = Nothing } else as)
