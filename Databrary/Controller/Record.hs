{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( getRecord
  , viewRecord
  , createRecord
  , postRecordMeasure
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Action.Route
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Permission
import Databrary.Controller.Angular
import Databrary.View.Record

getRecord :: Permission -> Id Record -> AuthActionM Record
getRecord p i =
  checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: API -> Id Record -> AppRAction
viewRecord api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  rec <- getRecord PermissionPUBLIC i
  case api of
    JSON -> okResponse [] $ recordJSON rec
    HTML -> okResponse [] $ show $ recordId rec -- TODO

createRecord :: API -> Id Volume -> AppRAction
createRecord api vi = action POST (api, vi, "record" :: T.Text) $ withAuth $ do
  vol <- getVolume PermissionEDIT vi
  br <- runForm (api == HTML ?> htmlRecordForm vol) $ do
    cat <- "category" .:> (flatMapM ((`orElseM` Nothing <$ deformError "No such record category.") . getRecordCategory) =<< deformNonEmpty deform)
    return (blankRecord vol)
      { recordCategory = cat
      }
  rec <- addRecord br
  case api of
    JSON -> okResponse [] $ recordJSON rec
    HTML -> redirectRouteResponse [] $ viewRecord api $ recordId rec

postRecordMeasure :: API -> Id Record -> Id Metric -> AppRAction
postRecordMeasure api ri mi = action POST (api, ri, mi) $ withAuth $ do
  rec <- getRecord PermissionEDIT ri
  met <- maybeAction $ getMetric mi
  let meas = Measure rec met
  rec' <- runForm (api == HTML ?> htmlRecordMeasureForm rec met) $
    deformSync' ("datum" .:> deformNonEmpty deform)
    >>= maybe
      (lift $ removeRecordMeasure $ meas "")
      (\d -> do
        r <- lift $ changeRecordMeasure $ meas d
        when (isNothing r) $ deformError $ T.pack $ "Invalid " ++ show (metricType met)
        return $ fromMaybe rec r)
  case api of
    JSON -> okResponse [] $ recordJSON rec'
    HTML -> redirectRouteResponse [] $ viewRecord api $ recordId rec'
