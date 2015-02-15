{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Authorize.SQL
  ( selectAuthorizeParent
  , selectAuthorizeChild
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.Time.Types
import Databrary.Model.SQL
import Databrary.Model.Party.SQL (selectParty)
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Permission.SQL
import Databrary.Model.Authorize.Types

makeAuthorize :: Access -> Maybe Timestamp -> Party -> Party -> Authorize
makeAuthorize a e c p = Authorize
  { authorization = Authorization
    { authorizeAccess = a
    , authorizeChild = c
    , authorizeParent = p
    }
  , authorizeExpires = e
  }

authorizeRow :: Selector -- @'Party' -> 'Party' -> 'Authorize'@
authorizeRow = addSelects 'makeAuthorize
  (accessRow "authorize") ["expires"]

selectAuthorizeParent :: TH.Name -- ^ child 'Party'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'Authorize'
selectAuthorizeParent child ident = selectJoin '($)
  [ selectMap (`TH.AppE` TH.VarE child) authorizeRow
  , joinOn ("authorize.parent = party.id AND authorize.child = ${partyId " ++ nameRef child ++ "}") 
    $ selectParty ident
  ]

selectAuthorizeChild :: TH.Name -- ^ parent 'Party'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'Authorize'
selectAuthorizeChild parent ident = selectMap (`TH.AppE` TH.VarE parent) $ selectJoin '($)
  [ authorizeRow
  , joinOn ("authorize.child = party.id AND authorize.parent = ${partyId " ++ nameRef parent ++ "}") 
    $ selectParty ident
  ]
