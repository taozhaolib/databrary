{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.VolumeAccess
  ( VolumeAccessTarget(..)
  , viewVolumeAccess
  , postVolumeAccess
  ) where

import Databrary.Ops
import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import qualified Databrary.Web.Route as R
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Action.Route
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Angular
import Databrary.View.VolumeAccess

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

instance R.Routable VolumeAccessTarget where
  route = "access" >> VolumeAccessTarget . Id <$> R.route
  toRoute (VolumeAccessTarget (Id i)) = "access" : R.toRoute i

viewVolumeAccess :: Id Volume -> VolumeAccessTarget -> AppRAction
viewVolumeAccess vi at@(VolumeAccessTarget ap) = action GET (HTML, toRoute vi ++ toRoute at) $ withAuth $ do
  angular
  v <- getVolume PermissionADMIN vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  blankForm (htmlVolumeAccessForm a)

postVolumeAccess :: API -> Id Volume -> VolumeAccessTarget -> AppRAction
postVolumeAccess api vi at@(VolumeAccessTarget ap) = action POST (api, vi, at) $ withAuth $ do
  v <- getVolume PermissionADMIN vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  u <- peek
  let su = identitySuperuser u
      restr = unId ap <= 0
  a' <- runForm (api == HTML ?> htmlVolumeAccessForm a) $ do
    delete <- "delete" .:> deform
    let del
          | delete = return PermissionNONE
          | otherwise = deform
    individual <- "individual" .:> (del
      >>= deformCheck "Cannot share full access." ((||) (not restr) . (PermissionSHARED >=))
      >>= deformCheck "Cannot remove your ownership." ((||) (su || not (volumeAccessProvidesADMIN a)) . (PermissionADMIN <=)))
    children <- "children" .:> (del
      >>= deformCheck "Inherited access must not exceed individual." (individual >=)
      >>= deformCheck "You are not authorized to share data." ((||) (accessSite u >= PermissionEDIT) . (PermissionNONE ==)))
    return a
      { volumeAccessIndividual = individual
      , volumeAccessChildren = children
      }
  changeVolumeAccess a'
  case api of
    JSON -> okResponse [] $ JSON.Object $ volumeAccessJSON a
    HTML -> redirectRouteResponse [] $ viewVolumeAccess vi at
