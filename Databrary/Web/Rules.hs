{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import System.IO (stderr)
import System.Posix.FilePath ((</>), (<.>), splitExtension)

import Databrary.Ops
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Constants
import Databrary.Web.Routes
import Databrary.Web.Templates
import Databrary.Web.Messages
import Databrary.Web.Coffee
import Databrary.Web.Libs

generateWebFile :: WebGenerator
generateWebFile f@"constants.json" t = lift $ maybe (generateConstantsJSON f) (const $ return False) t
generateWebFile f@"constants.js" t = lift $ maybe (generateConstantsJS f) (const $ return False) t
generateWebFile f@"routes.js" t = lift $ maybe (generateRoutesJS f) (const $ return False) t
generateWebFile f@"messages.js" t = lift $ generateMessagesJS f t
generateWebFile f@"templates.js" t = generateTemplatesJS f t
generateWebFile f t =
  webLinkFile "web" f t
  <|> generateCoffeeJS f t
  <|> generateLib f t

regenerateWebFile :: RawFilePath -> IO (Maybe RawFilePath)
regenerateWebFile f = do
  _ <- removeFile (webDir </> f)
  r <- runMaybeT $ generateWebFile f Nothing
  when (isNothing r) $
    BSC.hPutStrLn stderr ("regenerateWebFile: " <> f)
  return $ f <$ r

generateWebFiles :: IO [RawFilePath]
generateWebFiles = do
  wd <- webDataFiles
  mapMaybeM regenerateWebFile $
    wd ++ 
    [ b <.> ".js" | (b, ".coffee") <- map splitExtension wd ] ++
    [ "constants.json", "constants.js", "routes.js", "messages.js", "templates.js" ]
