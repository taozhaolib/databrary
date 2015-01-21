{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SQL.Authorize
  ( accessRow
  , partyAuthorizationSelector
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.SQL.Party (partySelector)
import Databrary.Model.Types.Party
import Databrary.Model.Types.Authorize

accessRow :: String -> Selector
accessRow table = selectColumns 'Access table ["site", "member"]

makePartyAuthorization :: Access -> Party -> Party -> Authorization
makePartyAuthorization a p c = Authorization a c p

partyAuthorizationSelector :: TH.Name -> Selector
partyAuthorizationSelector child =
  selectMap (`TH.AppE` TH.VarE child) $ selectJoin 'makePartyAuthorization
    [ accessRow "authorize_view"
    , joinOn ("authorize_view.parent = party.id AND authorize_view.child = ${partyId " ++ TH.nameBase child ++ "}") partySelector 
    ]
