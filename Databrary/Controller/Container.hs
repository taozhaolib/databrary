{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  , viewContainer
  , createContainer
  , postContainer
  , containerDownloadName
  ) where

import Data.Maybe (maybeToList)
import qualified Data.Text as T

import Databrary.Ops
import qualified Databrary.Iso as I
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import {-# SOURCE #-} Databrary.Controller.Slot
import Databrary.View.Container

getContainer :: Permission -> Id Slot -> AuthActionM Container
getContainer p (Id (SlotId i s))
  | segmentFull s = checkPermission p =<< maybeAction =<< lookupContainer i
  | otherwise = result =<< notFoundResponse

containerDownloadName :: Container -> [T.Text]
containerDownloadName c = maybeToList $ containerName c

viewContainer :: AppRoute (API, Id Container)
viewContainer = I.second (slotContainerId . unId I.:<->: containerSlotId) I.<$> viewSlot

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

createContainer :: AppRoute (API, Id Volume)
createContainer = action POST (pathAPI </> pathId </< "slot") $ \(api, vi) -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  bc <- runForm (api == HTML ?> htmlContainerForm (Left vol)) $ do
    top <- "top" .:> deform
    containerForm (blankContainer vol)
      { containerTop = top }
  c <- addContainer bc
  case api of
    JSON -> okResponse [] $ containerJSON c
    HTML -> redirectRouteResponse [] viewContainer (api, containerId c) []

postContainer :: AppRoute (API, Id Slot)
postContainer = action POST (pathAPI </> pathSlotId) $ \(api, ci) -> withAuth $ do
  c <- getContainer PermissionEDIT ci
  c' <- runForm (api == HTML ?> htmlContainerForm (Right c)) $ containerForm c
  changeContainer c'
  case api of
    JSON -> okResponse [] $ containerJSON c'
    HTML -> redirectRouteResponse [] viewSlot (api, ci) []

