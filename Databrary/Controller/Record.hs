{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( viewRecord
  ) where

import Databrary.Action.Route
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Controller.Permission

withRecord :: Permission -> Id Record -> (Record -> AuthAction) -> AppAction
withRecord p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: API -> Id Record -> AppRAction
viewRecord api i = action GET (api, i) $
  withRecord PermissionPUBLIC i $
    case api of
      JSON -> okResponse [] . recordJSON
      HTML -> okResponse [] . show . recordId -- TODO
