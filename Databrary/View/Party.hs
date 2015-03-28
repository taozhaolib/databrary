{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Party
  ( htmlPartyForm
  , htmlPartySearchForm
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Action.Auth
import Databrary.Action
import Databrary.View.Form
import Databrary.Model.Party

import {-# SOURCE #-} Databrary.Controller.Party

htmlPartyForm :: Maybe Party -> AuthRequest -> FormHtml
htmlPartyForm p req = htmlForm (maybe "Create party" ((T.append "Edit ") . partyName) p) (maybe (createParty HTML) (postParty HTML . TargetParty . partyId) p) req $ do
  field "prename" $ inputText $ partyPreName =<< p
  field "name" $ inputText $ partySortName <$> p
  field "affiliation" $ inputText $ partyAffiliation =<< p
  field "url" $ inputText $ show <$> (partyURL =<< p)

htmlPartySearchForm :: PartyFilter -> AuthRequest -> FormHtml
htmlPartySearchForm pf req = htmlForm "Search users" (searchParty HTML) req $ do
  field "query" $ inputText $ partyFilterQuery pf
  field "access" $ inputEnum $ partyFilterAccess pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf
