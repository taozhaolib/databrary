{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , postParty
  , createParty
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Lens as Lens (set)
import Control.Monad (unless, (<=<))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Text.Digestive as Form

import Control.Has (view, see, peek, peeks)
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Web.Form
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Form

withParty :: Maybe Permission -> Id Party -> (AuthParty -> AuthAction) -> AppAction
withParty Nothing i f = withAuth $ do
  u <- peek
  unless (partyId u == i) $ result =<< forbiddenResponse
  f $ selfAuthParty u
withParty (Just p) i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupAuthParty i

displayParty :: Bool -> AuthParty -> AuthAction
displayParty True = okResponse [] <=< peeks . authPartyJSON
displayParty False = okResponse [] . partyName . see -- TODO

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
  perm :: Permission <- peek
  _ <- checkPermission PermissionADMIN perm
  let disp = displayPartyForm api Nothing
  (bp, _) <- runForm "party" disp (partyForm Nothing)
  p <- addParty bp
  displayParty api p
