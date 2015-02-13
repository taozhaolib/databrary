{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , postParty
  , createParty
  , searchParty
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Control.Lens as Lens (set, (^.))
import Control.Monad (unless)
import Control.Monad.Reader (reader)
import qualified Data.Aeson.Types as JSON
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Text.Digestive as Form

import Control.Has (view, see, peek)
import Databrary.Action
import Databrary.Model.Kind
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party
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

displayParty :: Bool -> Party -> AuthAction
displayParty True = okResponse [] . partyJSON
displayParty False = okResponse [] . partyName -- TODO

viewParty :: Bool -> Id Party -> AppRAction
viewParty api i = action GET (apiRoute api $ toRoute i) $
  withParty (Just PermissionNONE) i $
    displayParty api

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
    (p', _) <- runForm "party" disp (partyForm $ Just $ see p)
    changeParty p'
    displayParty api $ Lens.set view p' p

createParty :: Bool -> AppRAction
createParty api = action POST (apiRoute api [kindOf emptyParty]) $ withAuth $ do
  perm <- reader (Lens.^. accessPermission)
  _ <- checkPermission PermissionADMIN perm
  let disp = displayPartyForm api Nothing
  (bp, _) <- runForm "party" disp (partyForm Nothing)
  p <- addParty bp
  displayParty api p

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
