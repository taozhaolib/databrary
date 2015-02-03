{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Party.SQL
  ( changeQuery
  , partyRow
  , partySelector
  , accountSelector
  ) where

import Data.Char (toLower)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL (Selector, selectColumns, selectJoin, joinOn, maybeJoinOn)
import Databrary.Model.Audit.SQL (auditChangeQuery)
import Databrary.Model.Party.Types

changeQuery :: TH.Name -> TH.ExpQ
changeQuery p = auditChangeQuery "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ " " ++ ps ++ "}")) ["Name", "Affiliation", "URL"])
  ("id = ${partyId " ++ ps ++ "}")
  Nothing
  where ps = TH.nameBase p -- show $ TH.pprExp 3 $ TH.VarE p

partyRow :: Selector
partyRow = selectColumns 'Party "party" ["id", "name", "affiliation", "url"]

accountRow :: Selector
accountRow = selectColumns 'Account "account" ["email", "password"]

makeParty :: (Maybe Account -> Party) -> Maybe (Party -> Account) -> Party
makeParty pc ac = p where
  p = pc (fmap ($ p) ac)

partySelector :: Selector
partySelector = selectJoin 'makeParty 
  [ partyRow
  , maybeJoinOn "party.id = account.id" accountRow
  ]

makeAccount :: (Maybe Account -> Party) -> (Party -> Account) -> Account
makeAccount pc ac = a where
  a = ac (pc (Just a))

accountSelector :: Selector
accountSelector = selectJoin 'makeAccount 
  [ partyRow
  , joinOn "party.id = account.id" accountRow
  ]
