{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Party.Boot
  ( loadParty
  ) where

import Data.Maybe (fromJust)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL

useTPG

loadParty :: Id Party -> Permission -> TH.ExpQ -- ^ @'Permission' -> 'Party'@
loadParty i perm = do
  p <- dbQuery1 $(selectQuery partyRow "$WHERE party.id = ${i}")
  TH.lift $ fromJust p Nothing perm
