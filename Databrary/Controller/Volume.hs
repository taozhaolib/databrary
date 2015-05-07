{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( getVolume
  , viewVolume
  , viewVolumeForm
  , postVolume
  , createVolume
  , viewVolumeLinks
  , postVolumeLinks
  , queryVolumes
  , volumeDownloadName
  ) where

import Control.Applicative (Applicative, (<*>), pure, optional)
import Control.Monad (mfilter, guard, void, when, liftM2)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (peeks, peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.HTTP.Client (HTTPClientM)
import Databrary.Model.Enum
import Databrary.Model.Id
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
import Databrary.Model.Excerpt
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.View.Volume

getVolume :: Permission -> Id Volume -> AuthActionM Volume
getVolume p i =
  checkPermission p =<< maybeAction =<< lookupVolume i

volumeJSONField :: (MonadDB m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("party" JSON..= partyJSON (volumeAccessParty va)))
    <$> lookupVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField vol "citation" _ =
  Just . maybe JSON.Null JSON.toJSON <$> lookupVolumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toJSON <$> lookupVolumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.toJSON . map fundingJSON <$> lookupVolumeFunding vol
volumeJSONField vol "containers" _ =
  Just . JSON.toJSON . map containerJSON <$> lookupVolumeContainers vol
volumeJSONField vol "records" _ =
  Just . JSON.toJSON . map recordJSON <$> lookupVolumeRecords vol
volumeJSONField o "excerpts" _ =
  Just . JSON.toJSON . map excerptJSON <$> lookupVolumeExcerpts o
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: (MonadDB m, MonadHasIdentity c m) => Volume -> JSON.Query -> m JSON.Object
volumeJSONQuery vol = JSON.jsonQuery (volumeJSON vol) (volumeJSONField vol)

volumeDownloadName :: (MonadDB m, MonadHasIdentity c m) => Volume -> m [T.Text]
volumeDownloadName v = do
  owns <- lookupVolumeAccess v PermissionADMIN
  return $ (T.pack $ "databrary" ++ show (volumeId v))
    : map (partySortName . volumeAccessParty) owns
    ++ [fromMaybe (volumeName v) (getVolumeAlias v)]

viewVolume :: AppRoute (API, Id Volume)
viewVolume = action GET (pathAPI </> pathId) $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionPUBLIC vi
  case api of
    JSON -> okResponse [] =<< volumeJSONQuery v =<< peeks Wai.queryString
    HTML -> okResponse [] $ volumeName v -- TODO

citationForm :: (Functor m, Applicative m, Monad m) => DeformT m Citation
citationForm = Citation
  <$> ("head" .:> deform)
  <*> ("url" .:> deformNonEmpty deform)
  <*> ("year" .:> deformNonEmpty deform)
  <*> pure Nothing

volumeForm :: (Functor m, Monad m) => Volume -> DeformT m Volume
volumeForm v = do
  name <- "name" .:> deform
  alias <- "alias" .:> deformNonEmpty deform
  body <- "body" .:> deformNonEmpty deform
  return v
    { volumeName = name
    , volumeAlias = alias
    , volumeBody = body
    }

volumeCitationForm :: HTTPClientM c m => Volume -> DeformT m (Volume, Maybe Citation)
volumeCitationForm v = do
  vol <- volumeForm v
  cite <- "citation" .:> citationForm
  look <- flatMapM (lift . lookupCitation) $
    guard (T.null (volumeName vol) || T.null (citationHead cite) || isNothing (citationYear cite)) >> citationURL cite
  let fill = maybe cite (cite <>) look
      empty = isNothing (citationURL fill) && isNothing (citationYear fill)
      name 
        | Just title <- citationTitle fill
        , T.null (volumeName vol) = title
        | otherwise = volumeName vol
  _ <- "name" .:> deformRequired name
  when (not empty) $ void $
    "citation" .:> "name" .:> deformRequired (citationHead fill)
  return (vol{ volumeName = name }, empty ?!> fill)

viewVolumeForm :: AppRoute (Id Volume)
viewVolumeForm = action GET (pathHTML >/> pathId </< "edit") $ \vi -> withAuth $ do
  angular
  v <- getVolume PermissionEDIT vi
  blankForm . htmlVolumeForm (Just v) =<< lookupVolumeCitation v

postVolume :: AppRoute (API, Id Volume)
postVolume = action POST (pathAPI </> pathId) $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  cite <- lookupVolumeCitation v
  (v', cite') <- runForm (api == HTML ?> htmlVolumeForm (Just v) cite) $ volumeCitationForm v
  changeVolume v'
  _ <- changeVolumeCitation v' cite'
  case api of
    JSON -> okResponse [] $ volumeJSON v'
    HTML -> redirectRouteResponse [] viewVolume arg []

createVolume :: AppRoute API
createVolume = action POST (pathAPI </< "volume") $ \api -> withAuth $ do
  u <- peek
  (bv, cite, owner) <- runForm (api == HTML ?> htmlVolumeForm Nothing Nothing) $ do
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
  _ <- changeVolumeCitation v cite
  _ <- changeVolumeAccess $ VolumeAccess PermissionADMIN PermissionEDIT owner v
  case api of
    JSON -> okResponse [] $ volumeJSON v
    HTML -> redirectRouteResponse [] viewVolume (api, volumeId v) []

viewVolumeLinks :: AppRoute (Id Volume)
viewVolumeLinks = action GET (pathHTML >/> pathId </< "link") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  blankForm . htmlVolumeLinksForm v =<< lookupVolumeLinks v

postVolumeLinks :: AppRoute (API, Id Volume)
postVolumeLinks = action POST (pathAPI </> pathId </< "link") $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  links <- lookupVolumeLinks v
  links' <- runForm (api == HTML ?> htmlVolumeLinksForm v links) $
    withSubDeforms citationForm
  changeVolumeLinks v links'
  case api of
    JSON -> okResponse [] $ volumeJSON v JSON..+ ("links" JSON..= links')
    HTML -> redirectRouteResponse [] viewVolume arg []

volumeSearchForm :: (Applicative m, Monad m) => DeformT m VolumeFilter
volumeSearchForm = VolumeFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("party" .:> optional deform)

queryVolumes :: AppRoute API
queryVolumes = action GET (pathAPI </< "volume") $ \api -> withAuth $ do
  when (api == HTML) angular
  (vf, (limit, offset)) <- runForm (api == HTML ?> htmlVolumeSearchForm mempty) $
    liftM2 (,) volumeSearchForm paginationForm
  p <- findVolumes vf limit offset
  case api of
    JSON -> okResponse [] $ JSON.toJSON $ map volumeJSON p
    HTML -> blankForm $ htmlVolumeSearchForm vf
