{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import Data.Version (showVersion)
import qualified Network.Wai.Handler.Warp as Warp

import Paths_databrary (version)
import Databrary.Service.Init
import Databrary.Action (runAppRoute)
import Databrary.Routes (routeMap)

main :: IO ()
main = do
  conf <- loadConfig
  port <- C.require conf "port"
  routes <- evaluate routeMap
  withService conf $ Warp.runSettings
    ( Warp.setPort port
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.defaultSettings)
    . runAppRoute routes
