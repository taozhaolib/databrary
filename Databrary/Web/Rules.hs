{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Monad (when, mzero, msum)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import System.IO (hPutStrLn, stderr)

import Databrary.Ops
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Constants
import Databrary.Web.Routes
import Databrary.Web.Templates
import Databrary.Web.Messages
import Databrary.Web.Coffee
import Databrary.Web.Uglify
import Databrary.Web.Stylus
import Databrary.Web.Libs

fixedGenerators :: [(FilePath, WebGenerator)]
fixedGenerators =
  [ ("constants.json", generateConstantsJSON)
  , ("constants.js",   generateConstantsJS)
  , ("routes.js",      generateRoutesJS)
  , ("messages.js",    generateMessagesJS)
  , ("templates.js",   generateTemplatesJS)
  , ("app.min.js",     generateUglifyJS)
  ]

generateFixed :: WebGenerator
generateFixed f
  | Just g <- lookup (webFileRel f) fixedGenerators = g f
  | otherwise = mzero

generateStatic :: WebGenerator
generateStatic = fileNewer

generateWebFile :: WebGenerator
generateWebFile f = msum $ map ($ f)
  [ generateFixed
  , generateCoffeeJS
  , generateStylusCSS
  , generateLib
  , generateStatic
  ]

regenerateWebFile :: WebFilePath -> IO (Maybe WebFilePath)
regenerateWebFile f = do
  r <- runMaybeT $ runReaderT (generateWebFile f) Nothing
  when (isNothing r) $
    hPutStrLn stderr ("regenerateWebFile: " <> webFileRel f)
  return $ f <$ r

generateWebFiles :: IO ()
generateWebFiles = do
  wd <- allWebFiles
  mapM_ regenerateWebFile $
    [ b <.> ".js" | (b, ".coffee") <- map splitWebFileExtensions wd ] ++
    allWebLibs False ++
    [ "constants.json", "constants.js", "routes.js", "messages.js", "templates.js", "app.min.js", "app.min.css" ]
