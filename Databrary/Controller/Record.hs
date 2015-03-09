{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( viewRecord
  , createRecord
  , postRecordMeasure
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T

import Control.Applicative.Ops
import Databrary.Action.Route
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
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
createRecord api vi = action POST (api, vi, "record" :: T.Text) $
  withVolume PermissionEDIT vi $ \vol -> do
    br <- runForm (api == HTML ?> htmlRecordForm vol) $ do
      cat <- "category" .:> (flatMapM ((`orElseM` deformErrorDef Nothing "No such record category.") . getRecordCategory) =<< deform)
      return (blankRecord vol)
        { recordCategory = cat
        }
    rec <- addRecord br
    case api of
      JSON -> okResponse [] $ recordJSON rec
      HTML -> redirectRouteResponse [] $ viewRecord api $ recordId rec

postRecordMeasure :: API -> Id Record -> Id Metric -> AppRAction
postRecordMeasure api ri mi = action POST (api, ri, mi) $
  withRecord PermissionEDIT ri $ \rec -> do
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
