{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Party
  ( renderPartyForm
  , renderPartySearchForm
  ) where

import Data.Maybe (fromMaybe)

import Control.Applicative.Ops
import Databrary.Action
import Databrary.View.Form
import Databrary.Model.Party

renderPartyForm :: RouteAction q -> Maybe Party -> FormHtml
renderPartyForm act p = renderForm act $ do
  field "name" $ inputText $ partyName <$> p
  field "affiliation" $ inputText $ partyAffiliation =<< p
  field "url" $ inputText $ show <$> (partyURL =<< p)

renderPartySearchForm :: RouteAction q -> PartyFilter -> FormHtml
renderPartySearchForm act pf = renderForm act $ do
  field "query" $ inputText $ partyFilterQuery pf
  field "access" $ inputEnum $ partyFilterAccess pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf
