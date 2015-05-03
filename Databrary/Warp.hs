{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import Data.Version (showVersion)
import qualified Network.Wai.Handler.Warp as Warp

import Paths_databrary (version)
import Databrary.Service.Init (withService)
import Databrary.Service.Types (serviceConfig)
import Databrary.Action (runAppRoute)
import Databrary.Routes (routeMap)

main :: IO ()
main = withService $ \rc -> do
  let conf = serviceConfig rc
  port <- C.require conf "port"
  routes <- evaluate routeMap
  Warp.runSettings
    ( Warp.setPort port
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.defaultSettings)
    $ runAppRoute routes rc
