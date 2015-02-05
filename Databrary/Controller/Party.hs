{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , postParty
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Lens as Lens (set)
import Control.Monad (unless, (<=<))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Text.Digestive as Form

import Control.Has (view, see, peek, peeks)
import Databrary.Action
import Databrary.Action.Object
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Web.Form

withParty :: Maybe Permission -> Id Party -> (AuthParty -> AuthAction) -> AppAction
withParty Nothing i f = withAuth $ do
  u <- peek
  unless (partyId u == i) $ result =<< forbiddenResponse
  f $ selfAuthParty u
withParty (Just p) i f = withAuth $
  f =<< checkObject p (lookupAuthParty i)

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

postParty :: Bool -> Id Party -> AppRAction
postParty api i = action POST (apiRoute api $ toRoute i) $
  withParty (Just PermissionADMIN) i $ \p -> do
    (p', _) <- runForm "party" disp (partyForm $ Just $ see p)
    changeParty p'
    displayParty api $ Lens.set view p' p
  where disp = undefined
