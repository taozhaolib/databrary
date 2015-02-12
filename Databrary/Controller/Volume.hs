{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( viewVolume
  ) where

import Control.Applicative ((<$))
import Control.Monad (guard)
import Network.HTTP.Types (Query)
import qualified Network.Wai as Wai

import Control.Has (peeks)
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Controller.Permission

withVolume :: Permission -> Id Volume -> (Volume -> AuthAction) -> AppAction
withVolume p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupVolume i

displayVolume :: Maybe Query -> Volume -> AuthAction
displayVolume (Just q) vol = okResponse [] =<< volumeJSONQuery vol q
displayVolume Nothing vol = okResponse [] $ volumeName vol -- TODO

viewVolume :: Bool -> Id Volume -> AppRAction
viewVolume api i = action GET (apiRoute api $ toRoute i) $ do
  q <- peeks Wai.queryString
  withVolume PermissionPUBLIC i $
    displayVolume (q <$ guard api)
