{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Party.SQL
  ( partyRow
  , selectParty
  , selectAuthParty
  , selectSiteAuth
  , updateParty
  , insertParty
  , insertAccount
  ) where

import Data.Char (toLower)
import qualified Data.Foldable as Fold
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Audit.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Permission.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types

partyRow :: Selector -- ^ @Maybe 'Account' -> 'Permission' -> 'Party'
partyRow = selectColumns 'Party "party" ["id", "name", "affiliation", "url"]

accountRow :: Selector -- ^ @'Party' -> 'Account'@
accountRow = selectColumns 'Account "account" ["email", "password"]

makeParty :: (Maybe Account -> Permission -> Party) -> Maybe (Party -> Account) -> Permission -> Party
makeParty pc ac perm = p where
  p = pc (fmap ($ p) ac) perm

selectUnpermissionedParty :: Selector -- ^ @'Permission' -> 'Party'@
selectUnpermissionedParty = selectJoin 'makeParty 
  [ partyRow
  , maybeJoinUsing ["id"] accountRow
  ]

permissionParty :: (Permission -> Party) -> Maybe Permission -> Identity -> Party
permissionParty pf perm ident = pf $ if identitySuperuser ident
  then maxBound
  else maybe id max perm $ max PermissionPUBLIC $ min PermissionREAD $ accessSite ident

selectParty :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Party'@
selectParty ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` TH.ConE 'Nothing) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectUnpermissionedParty

selectAuthParty :: TH.Name -- ^ 'Identity`
  -> Selector -- ^ @'Party'@
selectAuthParty ident = selectMap (`TH.AppE` TH.VarE ident) $ selectJoin 'permissionParty
  [ selectUnpermissionedParty
  , maybeJoinOn ("party.id = authorize_valid.parent AND authorize_valid.child = ${view " ++ nameRef ident ++ " :: Id Party}")
    $ selector "authorize_valid" "LEAST(site, member)"
  ]

makeAccount :: (Maybe Account -> Permission -> Party) -> (Party -> Account) -> Permission -> Account
makeAccount pc ac perm = a where
  a = ac (pc (Just a) perm)

selectUnpermissionedAccount :: Selector -- ^ @'Permission' -> 'Account'@
selectUnpermissionedAccount = selectJoin 'makeAccount 
  [ partyRow
  , joinUsing ["id"] accountRow
  ]

makeSiteAuth :: (Permission -> Account) -> Maybe Access -> SiteAuth
makeSiteAuth p a = SiteAuth (p maxBound) (Fold.fold a)

selectSiteAuth :: Selector -- @'SiteAuth'@
selectSiteAuth = selectJoin 'makeSiteAuth
  [ selectUnpermissionedAccount
  , maybeJoinOn "party.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

updateParty :: TH.Name -> TH.ExpQ -- ()
updateParty p = auditUpdate "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  ("id = ${partyId " ++ ps ++ "}")
  Nothing
  where ps = nameRef p

insertParty :: TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @'Permission' -> 'Party'@
insertParty p = auditInsert "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  (Just $ OutputMap (`TH.AppE` TH.ConE 'Nothing) $ selectOutput partyRow)
  where ps = nameRef p

insertAccount :: TH.Name -- ^ @'Account'@
  -> TH.ExpQ
insertAccount a = auditInsert "account"
  [ ("id", "${partyId (accountParty " ++ as ++ ")}")
  , ("email", "${accountEmail " ++ as ++ "}")
  , ("password", "${accountPasswd " ++ as ++ "}")
  ] Nothing
  where as = nameRef a
