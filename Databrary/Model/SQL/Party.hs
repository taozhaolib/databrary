{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SQL.Party 
  ( changeQuery
  , rowSelector
  ) where

import Data.Char (toLower)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL (Selector, select)
import Databrary.Model.SQL.Audit (auditChangeQuery)
import Databrary.Model.Types.Party

changeQuery :: TH.ExpQ
changeQuery = auditChangeQuery "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ "}")) ["Name", "Affiliation", "URL"])
  "id = ${partyId}"
  Nothing

rowSelector :: Selector
rowSelector = select 'Party "party" ["id", "name", "affiliation", "url"]
