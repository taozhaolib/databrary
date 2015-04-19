{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Format.Boot
  ( loadFormats
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Format.SQL

useTPG

loadFormats :: TH.ExpQ
loadFormats =
  TH.lift =<< dbQuery $(selectQuery formatRow "ORDER BY id")
