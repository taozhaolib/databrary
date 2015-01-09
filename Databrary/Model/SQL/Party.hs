module Databrary.Model.SQL.Party 
  ( changeQuery
  ) where

import Data.Char (toLower)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Audit (auditChangeQuery)

changeQuery :: TH.ExpQ
changeQuery = auditChangeQuery "party"
  (map (\c -> (map toLower c, "${party" ++ c ++ "}")) ["Name", "Affiliation", "URL"])
  "id = ${partyId}"
  Nothing
