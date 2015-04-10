{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Excerpt
  ( postExcerpt
  , deleteExcerpt
  ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), conflict409)

import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Controller.Asset

excerptForm :: (Functor m, Applicative m, Monad m) => AssetSlot -> DeformT m AssetSegment
excerptForm a = AssetSegment a
  <$> ("segment" .:> deform)
  <*> (Just <$> ("classification" .:> (fromMaybe ClassificationPRIVATE <$> deformNonEmpty deform)))

postExcerpt :: Id Asset -> AppRAction
postExcerpt ai = action POST (JSON, ai, "excerpt" :: T.Text) $ withAuth $ do
  asset <- getAsset PermissionEDIT ai
  e <- runForm Nothing $ excerptForm asset
  r <- changeExcerpt e
  guardAction r $
    returnResponse conflict409 [] ("The requested excerpt overlaps an existing excerpt." :: T.Text)
  okResponse [] $ excerptJSON e

deleteExcerpt :: Id Asset -> AppRAction
deleteExcerpt ai = action DELETE (JSON, ai, "excerpt" :: T.Text) $ withAuth $ do
  asset <- getAsset PermissionEDIT ai
  e <- runForm Nothing $ excerptForm asset
  changeExcerpt e{ assetSegmentExcerpt = Nothing }
  okResponse [] $ assetSlotJSON asset
