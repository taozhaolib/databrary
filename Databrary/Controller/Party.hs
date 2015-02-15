{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , postParty
  , createParty
  , searchParty
  ) where

import Control.Applicative ((<$>), (<$), (<*>), pure)
import Control.Monad (unless, guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Text.Digestive as Form

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
import Databrary.Web.Form
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Form

withParty :: Maybe Permission -> Id Party -> (Party -> AuthAction) -> AppAction
withParty Nothing i f = withAuth $ do
  u <- peek
  unless (partyId u == i) $ result =<< forbiddenResponse
  f u
withParty (Just p) i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupAuthParty i

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

viewParty :: Bool -> Id Party -> AppRAction
viewParty api i = action GET (apiRoute api $ toRoute i) $ do
  q <- peeks Wai.queryString
  withParty (Just PermissionNONE) i $
    displayParty (q <$ guard api)

emptyParty :: Party
emptyParty = Party
  { partyId = error "new party"
  , partyName = ""
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  , partyPermission = PermissionREAD
  }

partyForm :: Monad m => Maybe Party -> Form.Form T.Text m Party
partyForm p = up
  <$> "name" Form..: Form.text (partyName <$> p)
  <*> "affiliation" Form..: Form.optionalText (partyAffiliation =<< p)
  <*> "url" Form..: Form.optionalText (partyURL =<< p) -- FIXME
  where
  up name affiliation url = (fromMaybe emptyParty p)
    { partyName = name
    , partyAffiliation = affiliation
    , partyURL = url
    }

displayPartyForm :: Bool -> Maybe (Id Party) -> Form.View T.Text -> AuthAction
displayPartyForm api i = displayForm api $
  renderForm (maybe (createParty api) (postParty api) i)
    [ ("name", inputText)
    , ("affiliation", inputText)
    , ("url", inputText)
    ]

postParty :: Bool -> Id Party -> AppRAction
postParty api i = action POST (apiRoute api $ toRoute i) $
  withParty (Just PermissionADMIN) i $ \p -> do
    let disp = displayPartyForm api (Just i)
    (p', _) <- runForm "party" disp (partyForm $ Just p)
    changeParty p'
    displayParty ([] <$ guard api) p'

createParty :: Bool -> AppRAction
createParty api = action POST (apiRoute api [kindOf emptyParty]) $ withAuth $ do
  perm <- peeks accessPermission'
  _ <- checkPermission PermissionADMIN perm
  let disp = displayPartyForm api Nothing
  (bp, _) <- runForm "party" disp (partyForm Nothing)
  p <- addParty bp
  displayParty ([] <$ guard api) p

paginationForm :: Monad m => Form.Form T.Text m (Int, Int)
paginationForm = (,)
  <$> "limit" Form..: checkReadForm "invalid limit" (\l -> l > 0 && l <= 129) (Just 32)
  <*> "offset" Form..: checkReadForm "invalid offset" (>= 0) (Just 0)

partySearchForm :: Monad m => Form.Form T.Text m PartyFilter
partySearchForm = PartyFilter
  <$> "query" Form..: Form.optionalString Nothing
  <*> "access" Form..: optionalEnumForm Nothing
  <*> "institution" Form..: (Just <$> Form.bool Nothing)
  <*> pure Nothing
  <*> pure Nothing

displayPartySearchForm :: Bool -> Form.View T.Text -> AuthAction
displayPartySearchForm api = displayForm api $
  renderForm (searchParty api)
    [ ("query", inputText)
    , ("access", inputEnum PermissionNONE)
    , ("institution", inputCheckbox)
    ]

searchParty :: Bool -> AppRAction
searchParty api = action GET (apiRoute api [kindOf emptyParty]) $ withAuth $ do
  let disp = displayPartySearchForm api
  ((pf, (limit, offset)), form) <- runForm "party" disp ((,) <$> partySearchForm <*> paginationForm)
  p <- findParties pf limit offset
  if api
    then okResponse [] $ JSON.toJSON $ map partyJSON p
    else displayPartySearchForm api form
