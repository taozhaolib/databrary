{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Party.SQL
  ( partyRow
  , selectParty
  , selectAccount
  , updateParty
  , insertParty
  ) where

import Data.Char (toLower)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Audit.SQL
import Databrary.Model.Party.Types

partyRow :: Selector -- ^ @Maybe 'Account' -> 'Party'
partyRow = selectColumns 'Party "party" ["id", "name", "affiliation", "url"]

accountRow :: Selector -- ^ @'Party' -> 'Account'@
accountRow = selectColumns 'Account "account" ["email", "password"]

makeParty :: (Maybe Account -> Party) -> Maybe (Party -> Account) -> Party
makeParty pc ac = p where
  p = pc (fmap ($ p) ac)

selectParty :: Selector -- ^ @'Party'@
selectParty = selectJoin 'makeParty 
  [ partyRow
  , maybeJoinUsing ["id"] accountRow
  ]

makeAccount :: (Maybe Account -> Party) -> (Party -> Account) -> Account
makeAccount pc ac = a where
  a = ac (pc (Just a))

selectAccount :: Selector -- ^ @'Account'@
selectAccount = selectJoin 'makeAccount 
  [ partyRow
  , joinUsing ["id"] accountRow
  ]

updateParty :: TH.Name -> TH.ExpQ -- ()
updateParty p = auditUpdate "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  ("id = ${partyId " ++ ps ++ "}")
  Nothing
  where ps = nameRef p

insertParty :: TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @'Party'@
insertParty p = auditInsert "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  (Just $ OutputMap (`TH.AppE` TH.ConE 'Nothing) $ selectOutput partyRow)
  where ps = nameRef p
