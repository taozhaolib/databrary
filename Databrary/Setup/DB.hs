module Main (main) where

import Databrary.Service.DB (withPGConnection)
import Databrary.Service.DB.Schema

main :: IO ()
main = withPGConnection $ updateDBSchema "schema"
