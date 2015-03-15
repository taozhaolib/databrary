{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( PartyTarget(..)
  , getParty
  , viewParty
  , viewPartyForm
  , postParty
  , createParty
  , searchParty
  ) where

import Control.Applicative (Applicative, (<*>), pure, (<|>), optional)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Action.Route
import Databrary.Action
import Databrary.DB
import Databrary.Model.Enum
import Databrary.Model.Kind
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import qualified Databrary.Web.Route as R
import Databrary.Web.Form.Deform
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
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

partyJSONField :: (DBM m, MonadHasIdentity c m) => Party -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
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

partyJSONQuery :: (DBM m, MonadHasIdentity c m) => Party -> JSON.Query -> m JSON.Object
partyJSONQuery p = JSON.jsonQuery (partyJSON p) (partyJSONField p)

viewParty :: API -> PartyTarget -> AppRAction
viewParty api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  p <- getParty (Just PermissionNONE) i
  case api of
    JSON -> okResponse [] =<< partyJSONQuery p =<< peeks Wai.queryString
    HTML -> okResponse [] $ partyName p -- TODO

partyForm :: (Functor m, Monad m) => Party -> DeformT m Party
partyForm p = do
  name <- "name" .:> (deformCheck "Required" (not . T.null) . T.strip =<< deform)
  affiliation <- "affiliation" .:> fmap T.strip <$> deform
  url <- "url" .:> deform
  return p
    { partyName = name
    , partyAffiliation = affiliation
    , partyURL = url
    }

viewPartyForm :: PartyTarget -> AppRAction
viewPartyForm i = action GET (HTML, i, "edit" :: T.Text) $ withAuth $ do
  angular
  p <- getParty (Just PermissionADMIN) i
  blankForm $ htmlPartyForm $ Just p

postParty :: API -> PartyTarget -> AppRAction
postParty api i = action POST (api, i) $ withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  p' <- runForm (api == HTML ?> htmlPartyForm (Just p)) $ partyForm p
  changeParty p'
  case api of
    JSON -> okResponse [] $ partyJSON p'
    HTML -> redirectRouteResponse [] $ viewParty api i

createParty :: API -> AppRAction
createParty api = action POST (api, kindOf blankParty :: T.Text) $ withAuth $ do
  perm <- peeks accessPermission'
  _ <- checkPermission PermissionADMIN perm
  bp <- runForm (api == HTML ?> htmlPartyForm Nothing) $ partyForm blankParty
  p <- addParty bp
  case api of
    JSON -> okResponse [] $ partyJSON p
    HTML -> redirectRouteResponse [] $ viewParty api $ TargetParty $ partyId p

paginationForm :: (Applicative m, Monad m) => DeformT m (Int, Int)
paginationForm = (,)
  <$> ("limit" .:> (deformCheck "Invalid limit" (\l -> l > 0 && l <= 129) =<< deform) <|> return 32)
  <*> ("offset" .:> (deformCheck "Invalid offset" (>= 0) =<< deform) <|> return 0)

partySearchForm :: (Applicative m, Monad m) => DeformT m PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deform)
  <*> ("access" .:> optional deform)
  <*> ("institution" .:> optional deform)
  <*> pure Nothing
  <*> pure Nothing

searchParty :: API -> AppRAction
searchParty api = action GET (api, "party" :: T.Text) $ withAuth $ do
  when (api == HTML) angular
  (pf, (limit, offset)) <- runForm (api == HTML ?> htmlPartySearchForm mempty) ((,) <$> partySearchForm <*> paginationForm)
  p <- findParties pf limit offset
  case api of
    JSON -> okResponse [] $ JSON.toJSON $ map partyJSON p
    HTML -> blankForm $ htmlPartySearchForm pf
