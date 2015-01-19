{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SQL.Authorize
  ( accessRow
  , rootAuthorizationSelector
  ) where

import Databrary.Model.SQL
import Databrary.Model.SQL.Party (partySelector)
import Databrary.Model.Party (Party, rootParty)
import Databrary.Model.Types.Authorize

accessRow :: String -> Selector
accessRow table = selectColumns 'Access table ["site", "member"]

makeRootAuthorization :: Access -> Party -> Authorization
makeRootAuthorization a c = Authorization a c rootParty

rootAuthorizationSelector :: Selector
rootAuthorizationSelector = selectJoin 'makeRootAuthorization
  [ accessRow "authorize_view"
  , joinOn "authorize_view.parent = 0 AND authorize_view.child = party.id" partySelector 
  ]
