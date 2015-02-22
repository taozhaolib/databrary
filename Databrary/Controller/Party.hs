{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , viewPartyForm
  , postParty
  , createParty
  , searchParty
  ) where

import Control.Applicative (Applicative, (<*>), pure, (<|>), optional)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (view, peek, peeks)
import qualified Databrary.JSON as JSON
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
import Databrary.View.Party

instance R.Routable (Maybe (Id Party)) where
  route = Just <$> R.route <|> Nothing <$ "profile"
  toRoute (Just i) = R.toRoute i
  toRoute Nothing = ["profile"]

withParty :: Maybe Permission -> Maybe (Id Party) -> (Party -> AuthAction) -> AppAction
withParty (Just p) (Just i) f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupAuthParty i
withParty _ mi f = withAuth $ do
  u <- peek
  unless (Fold.all (partyId u ==) mi) $ result =<< forbiddenResponse
  f u

partyJSONField :: (DBM m, MonadHasIdentity c m) => Party -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
partyJSONField p "parents" _ =
  Just . JSON.toJSON . map (\a ->
    authorizeJSON a JSON..+ ("party" JSON..= partyJSON (authorizeParent (authorization a))))
    <$> getAuthorizedParents p (view p >= PermissionADMIN)
partyJSONField p "children" _ =
  Just . JSON.toJSON . map (\a ->
    authorizeJSON a JSON..+ ("party" JSON..= partyJSON (authorizeChild (authorization a))))
    <$> getAuthorizedChildren p (view p >= PermissionADMIN)
partyJSONField p "volumes" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("volume" JSON..= volumeJSON (volumeAccessVolume va)))
    <$> partyVolumeAccess p (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
partyJSONField _ _ _ = return Nothing

partyJSONQuery :: (DBM m, MonadHasIdentity c m) => Party -> JSON.Query -> m JSON.Object
partyJSONQuery p = JSON.jsonQuery (partyJSON p) (partyJSONField p)

displayParty :: Maybe JSON.Query -> Party -> AuthAction
displayParty (Just q) p = okResponse [] =<< partyJSONQuery p q
displayParty Nothing p = okResponse [] $ partyName p -- TODO

viewParty :: Bool -> Maybe (Id Party) -> AppRAction
viewParty api i = action GET (apiRoute api $ toRoute i) $ do
  q <- peeks Wai.queryString
  withParty (Just PermissionNONE) i $
    displayParty (q <? api)

partyForm :: (Functor m, Monad m) => Party -> DeformT m Party
partyForm p = do
  name <- "name" .:> deform
  affiliation <- "affiliation" .:> deform
  url <- "url" .:> deform
  return p
    { partyName = name
    , partyAffiliation = affiliation
    , partyURL = url
    }

viewPartyForm :: Maybe (Id Party) -> AppRAction
viewPartyForm i = action GET (toRoute i ++ ["edit"]) $
  withParty (Just PermissionADMIN) i $
    blankForm . htmlPartyForm . Just

postParty :: Bool -> Maybe (Id Party) -> AppRAction
postParty api i = action POST (apiRoute api $ toRoute i) $
  withParty (Just PermissionADMIN) i $ \p -> do
    p' <- runForm (api ?!> htmlPartyForm (Just p)) $ partyForm p
    changeParty p'
    displayParty ([] <? api) p'

createParty :: Bool -> AppRAction
createParty api = action POST (apiRoute api [kindOf blankParty]) $ withAuth $ do
  perm <- peeks accessPermission'
  _ <- checkPermission PermissionADMIN perm
  bp <- runForm (api ?!> htmlPartyForm Nothing) $ partyForm blankParty
  p <- addParty bp
  displayParty (api ?> []) p

paginationForm :: (Applicative m, Monad m) => DeformT m (Int, Int)
paginationForm = (,)
  <$> ("limit" .:> (deformCheck "invalid limit" (\l -> l > 0 && l <= 129) =<< deform) <|> return 32)
  <*> ("offset" .:> (deformCheck "invalid offset" (>= 0) =<< deform) <|> return 0)

partySearchForm :: (Applicative m, Monad m) => DeformT m PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deform)
  <*> ("access" .:> optional deform)
  <*> ("institution" .:> optional deform)
  <*> pure Nothing
  <*> pure Nothing

searchParty :: Bool -> AppRAction
searchParty api = action GET (apiRoute api [kindOf blankParty]) $ withAuth $ do
  (pf, (limit, offset)) <- runForm (api ?!> htmlPartySearchForm mempty) ((,) <$> partySearchForm <*> paginationForm)
  p <- findParties pf limit offset
  if api
    then okResponse [] $ JSON.toJSON $ map partyJSON p
    else blankForm $ htmlPartySearchForm pf
