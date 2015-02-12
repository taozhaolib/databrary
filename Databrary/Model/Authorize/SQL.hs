{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Authorize.SQL
  ( accessRow
  , selectParentAuthorization
  , selectChildAuthorization
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Party.SQL (selectParty)
import Databrary.Model.Party.Types
import Databrary.Model.Authorize.Types

accessRow :: String -- ^ Table name
  -> Selector -- ^ 'Access'
accessRow table = selectColumns 'Access table ["site", "member"]

makePartyAuthorization :: Access -> Party -> Party -> Authorization
makePartyAuthorization a p c = Authorization a c p

selectParentAuthorization :: TH.Name -- ^ child 'Party'
  -> Selector -- ^ 'Authorization'
selectParentAuthorization child =
  selectMap (`TH.AppE` TH.VarE child) $ selectJoin 'makePartyAuthorization
    [ accessRow "authorize_view"
    , joinOn ("authorize_view.parent = party.id AND authorize_view.child = ${partyId " ++ nameRef child ++ "}") selectParty 
    ]

selectChildAuthorization :: TH.Name  -- ^ parent 'Party'
  -> Selector -- ^ 'Authorization'
selectChildAuthorization parent =
  selectMap (`TH.AppE` TH.VarE parent) $ selectJoin 'Authorization
    [ accessRow "authorize_view"
    , joinOn ("authorize_view.child = party.id AND authorize_view.parent = ${partyId " ++ nameRef parent ++ "}") selectParty
    ]
