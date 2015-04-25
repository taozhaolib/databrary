{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import Data.Version (showVersion)
import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Service (withService)
import Databrary.Service.Types (serviceConfig)
import Databrary.App (application)
import Paths_databrary (version)

main :: IO ()
main = withService $ \rc -> do
  let conf = serviceConfig rc
  port <- C.require conf "port"
  Warp.runSettings
    ( Warp.setPort port
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.defaultSettings)
    $ application rc
