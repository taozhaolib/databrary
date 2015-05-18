module Databrary.Setup.DB
  ( updateDB
  ) where

import Databrary.Service.DB (withPGConnection)
import Databrary.Service.DB.Schema

updateDB :: IO ()
updateDB = withPGConnection $ updateDBSchema "schema"
