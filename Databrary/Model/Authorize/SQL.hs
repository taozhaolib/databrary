{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Authorize.SQL
  ( selectAuthorizeParent
  , selectAuthorizeChild
  , updateAuthorize
  , insertAuthorize
  , deleteAuthorize
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.Time.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Party.SQL (selectParty)
import Databrary.Model.Audit.SQL
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

authorizeSets :: String -- ^ @'Authorize'@
  -> [(String, String)]
authorizeSets a =
  [ ("site", "${accessSite " ++ a ++ "}")
  , ("member", "${accessMember " ++ a ++ "}")
  , ("expires", "${authorizeExpires " ++ a ++ "}")
  ]

authorizeKeys :: String -- ^ @'Authorize'@
  -> [(String, String)]
authorizeKeys a =
  [ ("child", "${partyId (authorizeChild (authorization " ++ a ++ "))}")
  , ("parent", "${partyId (authorizeParent (authorization " ++ a ++ "))}")
  ]

updateAuthorize :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
updateAuthorize ident a = auditUpdate ident "authorize"
  (authorizeSets as)
  (whereEq $ authorizeKeys as)
  Nothing
  where as = nameRef a

insertAuthorize :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
insertAuthorize ident a = auditInsert ident "authorize"
  (authorizeKeys as ++ authorizeSets as)
  Nothing
  where as = nameRef a

deleteAuthorize :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
deleteAuthorize ident a = auditDelete ident "authorize"
  (whereEq $ authorizeKeys as)
  Nothing
  where as = nameRef a
