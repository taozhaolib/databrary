{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Authorize.SQL
  ( accessRow
  , parentAuthorizationSelector
  , childAuthorizationSelector
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Party.SQL (partySelector)
import Databrary.Model.Party.Types
import Databrary.Model.Authorize.Types

accessRow :: String -> Selector
accessRow table = selectColumns 'Access table ["site", "member"]

makePartyAuthorization :: Access -> Party -> Party -> Authorization
makePartyAuthorization a p c = Authorization a c p

parentAuthorizationSelector :: TH.Name -> Selector
parentAuthorizationSelector child =
  selectMap (`TH.AppE` TH.VarE child) $ selectJoin 'makePartyAuthorization
    [ accessRow "authorize_view"
    , joinOn ("authorize_view.parent = party.id AND authorize_view.child = ${partyId " ++ TH.nameBase child ++ "}") partySelector 
    ]

childAuthorizationSelector :: TH.Name -> Selector
childAuthorizationSelector parent =
  selectMap (`TH.AppE` TH.VarE parent) $ selectJoin 'Authorization
    [ accessRow "authorize_view"
    , joinOn ("authorize_view.child = party.id AND authorize_view.parent = ${partyId " ++ TH.nameBase parent ++ "}") partySelector
    ]
