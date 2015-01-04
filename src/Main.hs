{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Main (main) where

import qualified Data.ByteString as BS
import Snap.Core
import Snap.Http.Server.Config (defaultConfig)
import Snap.Snaplet
import Snap.Snaplet.PostgresqlTyped (pgInit)
import Snap.Util.FileServe (serveDirectory)

import App
import Paths_databrary (getDataDir)
import Databrary.Volume (volume)

routes :: [(BS.ByteString, Handler App App ())]
routes =
  [ ("/volume", pathArg volume)
  , ("/public", serveDirectory "public")
  ]

app :: SnapletInit App App
app = makeSnaplet "databrary" "Databrary" (Just getDataDir) $ do
  d <- nestSnaplet "db" db pgInit
  addRoutes routes
  return $ App d

main :: IO ()
main = serveSnaplet defaultConfig app
