{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Databrary.Controller.Volume
  ( withVolume
  , viewVolume
  , viewVolumeForm
  , postVolume
  , createVolume
  ) where

import Control.Applicative (Applicative, (<*>), pure)
import Control.Monad (mfilter, guard, when)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (peeks, peek)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Web.Client (HTTPClientM)
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
import Databrary.Model.Citation.CrossRef
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

citationForm :: (Functor m, Applicative m, Monad m) => DeformT m Citation
citationForm = Citation
  <$> ("head" .:> deform)
  <*> ("url" .:> deform)
  <*> ("year" .:> deform)
  <*> pure Nothing

volumeForm :: (Functor m, Monad m) => Volume -> DeformT m Volume
volumeForm v = do
  name <- "name" .:> deform
  alias <- "alias" .:> deform
  body <- "body" .:> deform
  return v
    { volumeName = name
    , volumeAlias = alias
    , volumeBody = body
    }

volumeCitationForm :: HTTPClientM c m => Volume -> DeformT m (Volume, Maybe Citation)
volumeCitationForm v = do
  vol <- volumeForm v
  cite <- "citation" .:> citationForm
  look <- maybe (return Nothing) (lift . lookupCitation) $
    guard (T.null (volumeName vol) || T.null (citationHead cite) || isNothing (citationYear cite)) >> citationURL cite
  let fill = maybe cite (cite <>) look
      empty = isNothing (citationURL fill) && isNothing (citationYear fill)
      name 
        | Just title <- citationTitle fill
        , T.null (volumeName vol) = title
        | otherwise = volumeName vol
  when (T.null name) $
    "name" .:> deformError "Required"
  when (not empty && T.null (citationHead fill)) $
    "citation" .:> "name" .:> deformError "Required"
  return (vol{ volumeName = name }, empty ?!> fill)

viewVolumeForm :: Id Volume -> AppRAction
viewVolumeForm vi = action GET (vi, "edit" :: T.Text) $
  withVolume PermissionEDIT vi $
    blankForm . htmlVolumeForm . Just

postVolume :: API -> Id Volume -> AppRAction
postVolume api vi = action POST (api, vi) $
  withVolume PermissionEDIT vi $ \v -> do
    (v', cite) <- runForm (api == HTML ?> htmlVolumeForm (Just v)) $ volumeCitationForm v
    changeVolume v'
    setVolumeCitation v' cite
    case api of
      JSON -> okResponse [] $ volumeJSON v'
      HTML -> redirectRouteResponse [] $ viewVolume api vi

createVolume :: API -> AppRAction
createVolume api = action POST (api, kindOf blankVolume :: T.Text) $ withAuth $ do
  u <- peek
  (bv, cite, owner) <- runForm (api == HTML ?> htmlVolumeForm Nothing) $ do
    (bv, cite) <- volumeCitationForm blankVolume
    own <- "owner" .:> do
      oi <- deformOptional deform
      own <- maybe (return $ Just $ selfAuthorize u) (lift . lookupAuthorizeParent u) oi
      deformMaybe' "You are not authorized to create volumes for that owner." $
        authorizeParent . authorization <$> mfilter ((PermissionADMIN <=) . accessMember) own
    auth <- lift $ lookupAuthorization own rootParty
    deformGuard "Insufficient site authorization to create volume." $
      PermissionEDIT <= accessSite auth
    return (bv, cite, own)
  v <- addVolume bv
  setVolumeCitation v cite
  case api of
    JSON -> okResponse [] $ volumeJSON v
    HTML -> redirectRouteResponse [] $ viewVolume api $ volumeId v
