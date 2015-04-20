{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Funding
  ( queryFunder
  , postVolumeFunding
  , deleteVolumeFunder
  ) where

import Control.Monad (liftM2)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE))

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Funding
import Databrary.Model.Funding.FundRef
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Volume

queryFunder :: AppRAction
queryFunder = action GET (JSON, "funder" :: T.Text) $ withAuth $ do
  _ <- authAccount
  (q, a) <- runForm Nothing $ liftM2 (,)
    ("query" .:> (deformRequired =<< deform))
    ("all" .:> deform)
  r <- if a
    then searchFundRef q
    else findFunders q
  okResponse [] $ JSON.toJSON $ map funderJSON r

postVolumeFunding :: Id Volume -> Id Funder -> AppRAction
postVolumeFunding vi fi = action POST (JSON, vi, fi) $ withAuth $ do
  v <- getVolume PermissionEDIT vi
  f <- maybeAction =<< lookupFunderRef fi
  a <- runForm Nothing $ do
    "awards" .:> filter (not . T.null) <$> withSubDeforms deform
  let fa = Funding f a
  _ <- changeVolumeFunding v fa
  okResponse [] $ fundingJSON fa

deleteVolumeFunder :: Id Volume -> Id Funder -> AppRAction
deleteVolumeFunder vi fi = action DELETE (JSON, vi, fi) $ withAuth $ do
  v <- getVolume PermissionEDIT vi
  _ <- removeVolumeFunder v fi
  okResponse [] $ volumeJSON v
