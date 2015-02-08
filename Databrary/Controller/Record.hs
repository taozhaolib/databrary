{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( viewRecord
  ) where

import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Controller.Permission

withRecord :: Permission -> Id Record -> (Record -> AuthAction) -> AppAction
withRecord p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupRecord i

displayRecord :: Bool -> Record -> AuthAction
displayRecord True = okResponse [] . recordJSON
-- displayRecord False = okResponse [] -- TODO

viewRecord :: Bool -> Id Record -> AppRAction
viewRecord api i = action GET (apiRoute api $ toRoute i) $
  withRecord PermissionPUBLIC i $
    displayRecord api
