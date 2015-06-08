{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

#ifdef VERSION_warp_tls
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (evaluate)
#ifndef DEVEL
import Control.Monad.Reader (runReaderT)
#endif
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import Data.Version (showVersion)
import qualified Network.Wai.Handler.Warp as Warp
#ifdef VERSION_warp_tls
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
#endif

import Paths_databrary (version
#ifndef DEVEL
  , getDataFileName)
import Databrary.Service.Types (serviceDB)
import Databrary.Service.DB.Schema (updateDBSchema)
#else
  )
#endif
import Databrary.Service.Init (loadConfig, withService)
import Databrary.Action (runAppRoute)
import Databrary.Routes (routeMap)

main :: IO ()
main = do
  conf <- loadConfig
  port <- C.require conf "port"
#ifdef VERSION_warp_tls
  cert <- C.lookup conf "ssl.cert"
  key <- C.lookup conf "ssl.key"
#endif
  routes <- evaluate routeMap
  withService conf $ \rc -> do
#ifndef DEVEL
    schema <- getDataFileName "schema"
    runReaderT (updateDBSchema schema) (serviceDB rc)
#endif
#ifdef VERSION_warp_tls
    maybe
      Warp.runSettings
      WarpTLS.runTLS
      (WarpTLS.tlsSettings <$> cert <*> key)
#else
    Warp.runSettings
#endif
      ( Warp.setPort port
      $ Warp.setTimeout 300
      $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
      $ Warp.defaultSettings)
      $ runAppRoute routes rc
