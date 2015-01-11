{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Main (main) where

import qualified Data.ByteString as BS
import Snap.Http.Server.Config (defaultConfig)
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectory)

import Paths_databrary (getDataDir)
import Databrary.App
import Databrary.Snaplet.PG (pgInit)
import Databrary.Snaplet.Entropy (entropyInit)
import Databrary.Model.Id (pathIdArg)
import Databrary.Model.Volume (testVolume)
import Databrary.Model.Party (testParty)

routes :: [(BS.ByteString, Handler App App ())]
routes =
  [ ("", pathIdArg testVolume)
  , ("", pathIdArg testParty)
  , ("/public", serveDirectory "public")
  ]

app :: SnapletInit App App
app = makeSnaplet "databrary" "Databrary" (Just getDataDir) $ do
  d <- nestSnaplet "db" db pgInit
  e <- nestSnaplet "entropy" entropy entropyInit
  addRoutes routes
  return $ App d e

main :: IO ()
main = serveSnaplet defaultConfig app
