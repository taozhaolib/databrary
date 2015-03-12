{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  ) where

import Databrary.Model.Permission
import Databrary.Model.Container

getContainer :: Permission -> Id Container -> AuthActionM Container
getContainer p i =
  checkPermission p =<< maybeAction =<< lookupContainer i

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

postContainer :: API -> Id Container -> AppRAction
postContainer api ci = action POST (api, ci) $ withAuth $ do
  c <- getContainer PermissionEDIT ci
  c' <- runForm (api == HTML ?> htmlContainerForm (Just c)) $ containerForm c
  changeContainer c'
  case api of
    JSON -> okResponse [] $ containerJSON c'
    HTML -> redirectRouteResponse [] $ viewSlot api ci

