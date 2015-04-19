{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Volume.Boot
  ( loadVolume
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL

useTPG

loadVolume :: Id Volume -> Permission -> TH.ExpQ -- ^ @'Volume'@
loadVolume i perm = do
  v <- dbQuery1' $(selectQuery volumeRow "WHERE volume.id = ${i}")
  TH.lift $ v perm
