{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( viewParty
  , postParty
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Text.Digestive as Form

import Control.Has (see, peek, peeks)
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

viewParty :: Bool -> Id Party -> AppRAction
viewParty api i = action GET (apiRoute api $ toRoute i) $
  withParty (Just PermissionNONE) i $ \p ->
    if api
      then okResponse [] =<< peeks (authPartyJSON p)
      else okResponse [] (partyName $ see p)

data PartyForm = PartyForm
  { partyFormName :: T.Text
  , partyFormAffiliation :: Maybe T.Text
  , partyFormURL :: Maybe T.Text
  }

partyForm :: Monad m => Maybe Party -> Form.Form T.Text m PartyForm
partyForm p = PartyForm
  <$> "name"        Form..: Form.text (partyName <$> p)
  <*> "affiliation" Form..: Form.optionalText (partyAffiliation =<< p)
  <*> "url"         Form..: Form.optionalText (partyURL =<< p) -- FIXME

postParty :: Bool -> Id Party -> AppRAction
postParty api i = action POST (apiRoute api $ toRoute i) $
  withParty (Just PermissionADMIN) i $ \p -> do
    (party, form) <- runForm "party" disp (partyForm $ Just $ see p)
    if api
      then okResponse [] =<< peeks (authPartyJSON p)
      else okResponse [] (partyName $ see p)
  where disp = undefined
