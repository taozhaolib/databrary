{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( viewVolume
  ) where

import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Controller.Permission

withVolume :: Permission -> Id Volume -> (Volume -> AuthAction) -> AppAction
withVolume p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupVolume i

displayVolume :: Bool -> Volume -> AuthAction
displayVolume True = okResponse [] . volumeJSON
displayVolume False = okResponse [] . volumeName -- TODO

viewVolume :: Bool -> Id Volume -> AppRAction
viewVolume api i = action GET (apiRoute api $ toRoute i) $
  withVolume PermissionPUBLIC i $
    displayVolume api
