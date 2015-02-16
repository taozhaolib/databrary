module Databrary.Controller.Asset
  ( uploadStart
  ) where

import Databrary.Controller.Volume

uploadStart :: Id Volume -> AppRAction
uploadStart vi = action POST (apiRoute True $ toRoute vi ++ ["asset"]) $ do
  withVolume PermissionEDIT vi $ \v -> do

