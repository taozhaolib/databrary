{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( PartyTarget(..)
  , getParty
  , viewParty
  , viewEditParty
  , postParty
  , createParty
  , queryParties
  , viewAvatar
  ) where

import Control.Applicative (Applicative, (<*>), pure, (<|>), optional)
import Control.Monad (unless, when, liftM2, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Action.Route
import Databrary.Action
import Databrary.Service.DB
import Databrary.Model.Enum
import Databrary.Model.Kind
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Asset
import Databrary.Model.Format
import Databrary.Store.Temp
import Databrary.Store.Asset
import qualified Databrary.HTTP.Route as R
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.File
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Web
import Databrary.View.Party

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

instance R.Routable PartyTarget where
  route = TargetParty <$> R.route <|> TargetProfile <$ "profile"
  toRoute (TargetParty i) = R.toRoute i
  toRoute TargetProfile = ["profile"]

getParty :: Maybe Permission -> PartyTarget -> AuthActionM Party
getParty (Just p) (TargetParty i) =
  checkPermission p =<< maybeAction =<< lookupAuthParty i
getParty _ mi = do
  u <- peek
  let isme TargetProfile = True
      isme (TargetParty i) = partyId u == i
  unless (isme mi) $ result =<< forbiddenResponse
  return u

partyJSONField :: (MonadDB m, MonadHasIdentity c m) => Party -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
partyJSONField p "parents" _ =
  Just . JSON.toJSON . map (\a ->
    authorizeJSON a JSON..+ ("party" JSON..= partyJSON (authorizeParent (authorization a))))
    <$> lookupAuthorizedParents p (view p >= PermissionADMIN)
partyJSONField p "children" _ =
  Just . JSON.toJSON . map (\a ->
    authorizeJSON a JSON..+ ("party" JSON..= partyJSON (authorizeChild (authorization a))))
    <$> lookupAuthorizedChildren p (view p >= PermissionADMIN)
partyJSONField p "volumes" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("volume" JSON..= volumeJSON (volumeAccessVolume va)))
    <$> lookupPartyVolumeAccess p (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
partyJSONField p "access" _ = do
  Just . JSON.toJSON . accessSite <$> lookupAuthorization p rootParty
partyJSONField _ _ _ = return Nothing

partyJSONQuery :: (MonadDB m, MonadHasIdentity c m) => Party -> JSON.Query -> m JSON.Object
partyJSONQuery p = JSON.jsonQuery (partyJSON p) (partyJSONField p)

viewParty :: API -> PartyTarget -> AppRAction
viewParty api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  p <- getParty (Just PermissionNONE) i
  case api of
    JSON -> okResponse [] =<< partyJSONQuery p =<< peeks Wai.queryString
    HTML -> okResponse [] $ partyName p -- TODO

processParty :: API -> Maybe Party -> AuthActionM (Party, Maybe Asset)
processParty api p = do
  (p', a) <- runFormFiles [("avatar", maxAvatarSize)] (api == HTML ?> htmlPartyForm p) $ do
    name <- "name" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    affiliation <- "affiliation" .:> deformNonEmpty deform
    url <- "url" .:> deformNonEmpty deform
    avatar <- "avatar" .:>
      (Trav.mapM (\a -> do
        f <- deformCheck "Must be an image." formatIsImage =<<
          deformMaybe' "Unknown or unsupported file format."
          (getFormatByFilename (fileName a))
        return (a, f)) =<< deform)
    return ((fromMaybe blankParty p)
      { partySortName = name
      , partyPreName = prename
      , partyAffiliation = affiliation
      , partyURL = url
      }, avatar)
  a' <- Trav.forM a $ \(af, fmt) -> do
    a' <- addAsset (blankAsset coreVolume)
      { assetFormat = fmt
      , assetClassification = ClassificationPUBLIC
      , assetName = Just $ TE.decodeUtf8 $ fileName af
      } $ Just $ tempFilePath (fileContent af)
    releaseTempFile $ fileContent af
    return a'
  return (p', a')
  where maxAvatarSize = 10*1024*1024

viewEditParty :: PartyTarget -> AppRAction
viewEditParty i = action GET (HTML, i, "edit" :: T.Text) $ withAuth $ do
  angular
  p <- getParty (Just PermissionADMIN) i
  blankForm $ htmlPartyForm $ Just p

postParty :: API -> PartyTarget -> AppRAction
postParty api i = multipartAction $ action POST (api, i) $ withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  (p', a) <- processParty api (Just p)
  changeParty p'
  when (isJust a) $
    void $ changeAvatar p' a
  case api of
    JSON -> okResponse [] $ partyJSON p'
    HTML -> redirectRouteResponse [] $ viewParty api i

createParty :: API -> AppRAction
createParty api = multipartAction $ action POST (api, kindOf blankParty :: T.Text) $ withAuth $ do
  perm <- peeks accessPermission'
  _ <- checkPermission PermissionADMIN perm
  (bp, a) <- processParty api Nothing
  p <- addParty bp
  when (isJust a) $
    void $ changeAvatar p a
  case api of
    JSON -> okResponse [] $ partyJSON p
    HTML -> redirectRouteResponse [] $ viewParty api $ TargetParty $ partyId p

partySearchForm :: (Applicative m, Monad m) => DeformT m PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("access" .:> optional deform)
  <*> ("institution" .:> optional deform)
  <*> pure Nothing
  <*> pure Nothing

queryParties :: API -> AppRAction
queryParties api = action GET (api, "party" :: T.Text) $ withAuth $ do
  when (api == HTML) angular
  (pf, (limit, offset)) <- runForm (api == HTML ?> htmlPartySearchForm mempty) $
    liftM2 (,) partySearchForm paginationForm
  p <- findParties pf limit offset
  case api of
    JSON -> okResponse [] $ JSON.toJSON $ map partyJSON p
    HTML -> blankForm $ htmlPartySearchForm pf

viewAvatar :: Id Party -> AppRAction
viewAvatar i = action GET (i, "avatar" :: T.Text) $
  maybe
    (redirectRouteResponse [] $ webFile $ staticPath ["images", "avatar.png"])
    (\a -> do
      -- elsewhere? size <- runForm Nothing $ "size" .:> optional deform
      store <- maybeAction =<< getAssetFile a
      serveFile store (assetFormat a) Nothing (fromJust $ assetSHA1 a))
    =<< lookupAvatar i
