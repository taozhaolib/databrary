{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  , createContainer
  , postContainer
  ) where

import qualified Data.Text as T

import Databrary.Ops
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.View.Container

getContainer :: Permission -> Id Slot -> AuthActionM Container
getContainer p (Id (SlotId i s))
  | segmentFull s = checkPermission p =<< maybeAction =<< lookupContainer i
  | otherwise = result =<< notFoundResponse

viewContainer :: API -> Id Container -> AppRAction
viewContainer api = viewSlot api . containerSlotId

containerForm :: (Functor m, Monad m) => Container -> DeformT m Container
containerForm c = do
  name <- "name" .:> deformNonEmpty deform
  date <- "date" .:> deformNonEmpty deform
  release <- "release" .:> deformNonEmpty deform
  return c
    { containerName = name
    , containerDate = date
    , containerRelease = release
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

postContainer :: API -> Id Slot -> AppRAction
postContainer api ci = action POST (api, ci) $ withAuth $ do
  c <- getContainer PermissionEDIT ci
  c' <- runForm (api == HTML ?> htmlContainerForm (Right c)) $ containerForm c
  changeContainer c'
  case api of
    JSON -> okResponse [] $ containerJSON c'
    HTML -> redirectRouteResponse [] $ viewSlot api ci

