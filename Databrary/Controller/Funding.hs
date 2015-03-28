{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Funding
  ( postVolumeFunding
  , deleteVolumeFunder
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE))

import Databrary.Ops
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Funding
import Databrary.Model.Funding.FundRef
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Controller.Volume

postVolumeFunding :: Id Volume -> Id Funder -> AppRAction
postVolumeFunding vi fi = action POST (JSON, vi, fi) $ withAuth $ do
  v <- getVolume PermissionEDIT vi
  f <- maybeAction =<< lookupFunderRef fi
  a <- runForm Nothing $ do
    "awards" .:> filter (not . T.null) <$> withSubDeforms (T.strip <$> deform)
  let fa = Funding f a
  changeVolumeFunding v fa
  okResponse [] $ fundingJSON fa

deleteVolumeFunder :: Id Volume -> Id Funder -> AppRAction
deleteVolumeFunder vi fi = action DELETE (JSON, vi, fi) $ withAuth $ do
  v <- getVolume PermissionEDIT vi
  removeVolumeFunder v fi
  okResponse [] $ volumeJSON v
