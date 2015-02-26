{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Party.Boot
  ( loadParty
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL

useTPG

loadParty :: Id Party -> Permission -> TH.ExpQ -- ^ @'Party'@
loadParty i perm = do
  Just p <- dbQuery1 $(selectQuery partyRow "$WHERE party.id = ${i}")
  TH.lift $ p Nothing perm Nothing
