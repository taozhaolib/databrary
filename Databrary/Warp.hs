{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

#ifdef VERSION_warp_tls
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (evaluate)
import Control.Monad (void)
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
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)

import Paths_databrary (version
#ifndef DEVEL
  , getDataFileName)
import Databrary.Service.Types (serviceDB)
import Databrary.Service.DB.Schema (updateDBSchema)
#else
  )
#endif
import Databrary.Service.Init (loadConfig, withService)
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runAppRoute)
import Databrary.Routes (routeMap)

data Flag
  = FlagWeb
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case Opt.getOpt Opt.Permute opts args of
    ([FlagWeb], [], []) -> do
      void generateWebFiles
      exitSuccess
    ([], [], []) -> return ()
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure
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
