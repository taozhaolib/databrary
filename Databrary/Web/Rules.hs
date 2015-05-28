{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Monad (when, mzero, msum)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import System.IO (hPutStrLn, stderr)

import Databrary.Ops
import Databrary.Store
import Databrary.Web
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
generateFixed f t
  | Just g <- lookup (webFileRel f) fixedGenerators = g f t
  | otherwise = mzero

generateStatic :: WebGenerator
generateStatic f t = maybe (const True) (<) t <$> webFileTime f

generateWebFile :: WebGenerator
generateWebFile f t = msum $ map (\g -> g f t)
  [ generateFixed
  , generateCoffeeJS
  , generateStylusCSS
  , generateLib
  , generateStatic
  ]

regenerateWebFile :: WebFilePath -> IO (Maybe WebFilePath)
regenerateWebFile f = do
  r <- runMaybeT $ generateWebFile f Nothing
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
