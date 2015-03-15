{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  , viewContainer
  , createContainer
  , postContainer
  ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.AssetSlot
import Databrary.Model.RecordSlot
import Databrary.Action
import Databrary.Web.Form.Deform
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Angular
import Databrary.View.Container

getContainer :: Permission -> Id Container -> AuthActionM Container
getContainer p i =
  checkPermission p =<< maybeAction =<< lookupContainer i

containerJSONField :: (DBM m, MonadHasIdentity c m) => Container -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
containerJSONField c "assets" _ =
  Just . JSON.toJSON . map assetSlotJSON <$> lookupContainerAssets c
containerJSONField c "records" _ =
  Just . JSON.toJSON . map recordSlotJSON <$> lookupContainerRecords c
containerJSONField _ _ _ = return Nothing

containerJSONQuery :: (DBM m, MonadHasIdentity c m) => Container -> JSON.Query -> m JSON.Object
containerJSONQuery vol = JSON.jsonQuery (containerJSON vol) (containerJSONField vol)

viewContainer :: API -> Id Container -> AppRAction
viewContainer api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  c <- getContainer PermissionPUBLIC i
  case api of
    JSON -> okResponse [] =<< containerJSONQuery c =<< peeks Wai.queryString
    HTML -> okResponse [] $ show $ containerId c -- TODO

containerForm :: (Functor m, Monad m) => Container -> DeformT m Container
containerForm c = do
  name <- "name" .:> fmap T.strip <$> deform
  date <- "date" .:> deform
  consent <- "consent" .:> deform
  return c
    { containerName = name
    , containerDate = date
    , containerConsent = consent
    }

createContainer :: API -> Id Volume -> AppRAction
createContainer api vi = action POST (api, vi, "slot" :: T.Text) $ withAuth $ do
  vol <- getVolume PermissionEDIT vi
  bc <- runForm (api == HTML ?> htmlContainerForm (Left vol)) $ do
    top <- "top" .:> deform
    containerForm (blankContainer vol)
      { containerTop = top }
  c <- addContainer bc
  case api of
    JSON -> okResponse [] $ containerJSON c
    HTML -> redirectRouteResponse [] $ viewContainer api $ containerId c

postContainer :: API -> Id Container -> AppRAction
postContainer api ci = action POST (api, ci) $ withAuth $ do
  c <- getContainer PermissionEDIT ci
  c' <- runForm (api == HTML ?> htmlContainerForm (Right c)) $ containerForm c
  changeContainer c'
  case api of
    JSON -> okResponse [] $ containerJSON c'
    HTML -> redirectRouteResponse [] $ viewContainer api ci

