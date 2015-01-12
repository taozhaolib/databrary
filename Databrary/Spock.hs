{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Spock (app) where

import Database.PostgreSQL.Typed (PGConnection, pgConnect, pgDisconnect)
import Web.Spock.Safe

import Databrary.PG
import Databrary.Model.Id (pathIdArg)
import Databrary.Model.Volume (testVolume)
import Databrary.Model.Party (testParty)

sessionCfg :: SessionCfg
sessionCfg = undefined

dbCfg :: PollOrConn PGConnection
dbCfg = PCConn $ ConnBuilder
  { cb_createConn = pgConnect pgDatabase
  , cb_destroyConn = pgDisconnect
  , cb_poolConfiguration = PoolCfg
    { pc_stripes = 1
    , pc_resPerStripe = 16
    , pc_keepOpenTime = 60
    }
  }

app :: Middleware
app = spock sessionCfg dbCfg () $ do
  get ("volume" <//> var) testVolume
  get ("party" <//> var) testParty

main :: IO ()
main = serveSnaplet defaultConfig app
