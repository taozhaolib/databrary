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

partyRow :: Selector
partyRow = selectColumns 'Party "party" ["id", "name", "affiliation", "url"]

accountRow :: Selector
accountRow = selectColumns 'Account "account" ["email", "password"]

makeParty :: (Maybe Account -> Party) -> Maybe (Party -> Account) -> Party
makeParty pc ac = p where
  p = pc (fmap ($ p) ac)

selectParty :: Selector
selectParty = selectJoin 'makeParty 
  [ partyRow
  , maybeJoinUsing ["id"] accountRow
  ]

makeAccount :: (Maybe Account -> Party) -> (Party -> Account) -> Account
makeAccount pc ac = a where
  a = ac (pc (Just a))

selectAccount :: Selector
selectAccount = selectJoin 'makeAccount 
  [ partyRow
  , joinUsing ["id"] accountRow
  ]

updateParty :: TH.Name -> TH.ExpQ
updateParty p = auditUpdate "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  ("id = ${partyId " ++ ps ++ "}")
  Nothing
  where ps = TH.nameBase p

insertParty :: TH.Name -> TH.ExpQ
insertParty p = auditInsert "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  (Just $ OutputMap (`TH.AppE` TH.ConE 'Nothing) $ selectOutput partyRow)
  where ps = TH.nameBase p
