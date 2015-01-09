{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Main (main) where

import qualified Data.ByteString as BS
import Snap.Http.Server.Config (defaultConfig)
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectory)

import Paths_databrary (getDataDir)
import Databrary.App
import Databrary.Snaplet.PG (pgInit)
import Databrary.Model.Id (pathIdArg)
import Databrary.Model.Volume (testVolume)
import Databrary.Model.Party ()

routes :: [(BS.ByteString, Handler App App ())]
routes =
  [ ("", pathIdArg testVolume)
  , ("/public", serveDirectory "public")
  ]

app :: SnapletInit App App
app = makeSnaplet "databrary" "Databrary" (Just getDataDir) $ do
  d <- nestSnaplet "db" db pgInit
  addRoutes routes
  return $ App d

main :: IO ()
main = serveSnaplet defaultConfig app
