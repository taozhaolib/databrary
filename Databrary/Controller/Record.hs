{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( viewRecord
  , createRecord
  ) where

import qualified Data.Text as T

import Control.Applicative.Ops
import Databrary.Action.Route
import Databrary.Action
import Databrary.Model.Kind
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Permission
import Databrary.View.Record

withRecord :: Permission -> Id Record -> (Record -> AuthAction) -> AppAction
withRecord p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: API -> Id Record -> AppRAction
viewRecord api i = action GET (api, i) $
  withRecord PermissionPUBLIC i $
    case api of
      JSON -> okResponse [] . recordJSON
      HTML -> okResponse [] . show . recordId -- TODO

createRecord :: API -> Id Volume -> AppRAction
createRecord api vi = action POST (api, vi, kindOf (blankRecord undefined) :: T.Text) $
  withVolume PermissionEDIT vi $ \vol -> do
    br <- runForm (api == HTML ?> htmlRecordForm vol) $ do
      cat <- "category" .:> (maybe (return Nothing) (maybe (deformErrorDef Nothing "No such record category.") (return . Just) . getRecordCategory) =<< deformNonempty deform)
      return (blankRecord vol)
        { recordCategory = cat
        }
    rec <- addRecord br
    case api of
      JSON -> okResponse [] $ recordJSON rec
      HTML -> redirectRouteResponse [] $ viewRecord api $ recordId rec
