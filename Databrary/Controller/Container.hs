{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  , viewContainer
  , createContainer
  , postContainer
  ) where

import Control.Monad (when)
import qualified Data.Text as T

import Control.Applicative.Ops
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
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

viewContainer :: API -> Id Container -> AppRAction
viewContainer api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  c <- getContainer PermissionPUBLIC i
  case api of
    JSON -> okResponse [] $ containerJSON c
    HTML -> okResponse [] $ show $ containerId c -- TODO

containerForm :: (Functor m, Monad m) => Container -> DeformT m Container
containerForm c = do
  name <- "name" .:> deform
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

