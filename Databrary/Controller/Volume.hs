{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( withVolume
  , viewVolume
  , viewVolumeForm
  , postVolume
  , createVolume
  ) where

import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (peeks, peek)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Enum
import Databrary.Model.Id
import Databrary.Model.Kind
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Citation
import Databrary.Model.Funding
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Web.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Volume

withVolume :: Permission -> Id Volume -> (Volume -> AuthAction) -> AppAction
withVolume p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupVolume i

volumeJSONField :: (DBM m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("party" JSON..= partyJSON (volumeAccessParty va)))
    <$> volumeVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField vol "citation" _ =
  Just . maybe JSON.Null JSON.toJSON <$> volumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toJSON <$> volumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.toJSON <$> volumeFunding vol
volumeJSONField vol "containers" _ =
  Just . JSON.toJSON . map containerJSON <$> volumeContainers vol
volumeJSONField vol "records" _ =
  Just . JSON.toJSON . map recordJSON <$> volumeRecords vol
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: (DBM m, MonadHasIdentity c m) => Volume -> JSON.Query -> m JSON.Object
volumeJSONQuery vol = JSON.jsonQuery (volumeJSON vol) (volumeJSONField vol)

viewVolume :: API -> Id Volume -> AppRAction
viewVolume api vi = action GET (api, vi) $
  withVolume PermissionPUBLIC vi $ \v ->
    case api of
      JSON -> okResponse [] =<< volumeJSONQuery v =<< peeks Wai.queryString
      HTML -> okResponse [] $ volumeName v -- TODO

volumeForm :: (Functor m, Monad m) => Volume -> DeformT m Volume
volumeForm v = do
  name <- "name" .:> (deformCheck "Required" (not . T.null) =<< deform)
  alias <- "alias" .:> deform
  body <- "body" .:> deformNonempty deform
  return v
    { volumeName = name
    , volumeAlias = alias
    , volumeBody = body
    }

viewVolumeForm :: Id Volume -> AppRAction
viewVolumeForm vi = action GET (vi, "edit" :: T.Text) $
  withVolume PermissionEDIT vi $
    blankForm . htmlVolumeForm . Just

postVolume :: API -> Id Volume -> AppRAction
postVolume api vi = action POST (api, vi) $
  withVolume PermissionEDIT vi $ \v -> do
    v' <- runForm (api == HTML ?> htmlVolumeForm (Just v)) $ volumeForm v
    changeVolume v'
    case api of
      JSON -> okResponse [] $ volumeJSON v'
      HTML -> redirectRouteResponse [] $ viewVolume api vi

createVolume :: API -> AppRAction
createVolume api = action POST (api, kindOf blankVolume :: T.Text) $ withAuth $ do
  u <- peek
  (bv, owner) <- runForm (api == HTML ?> htmlVolumeForm Nothing) $ do
    bv <- volumeForm blankVolume
    own <- "owner" .:> do
      oi <- deformOptional deform
      own <- maybe (return $ Just $ selfAuthorize u) (lift . lookupAuthorizeParent u) oi
      deformCheck "You are not authorized to create volumes with that owner."
        (Fold.any ((PermissionADMIN <=) . accessMember)) own
    return (bv, own)
  fail "TODO"
  v <- addVolume bv
  case api of
    JSON -> okResponse [] $ volumeJSON v
    HTML -> redirectRouteResponse [] $ viewVolume api $ volumeId v
