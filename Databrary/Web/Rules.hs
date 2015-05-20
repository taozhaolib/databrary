{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import System.Posix.FilePath ((</>), splitExtensions)

import Databrary.Store
import Databrary.Model.Time
import Databrary.Web.Files
import Databrary.Web.Constants
import Databrary.Web.Routes
import Databrary.Web.Templates
import Databrary.Web.Messages

jsLibs :: [(RawFilePath, FilePath)]
jsLibs =
  [ ("jquery",              "bower_components/jquery/dist")
  , ("angular",             "bower_components/angular")
  , ("angular-route",       "bower_components/angular-route")
  , ("ng-flow-standalone",  "bower_components/ng-flow/dist")
  , ("lodash",              "bower_components/lodash")
  ]

generateWebFile :: RawFilePath -> Maybe Timestamp -> IO (Maybe Bool)
generateWebFile f@"constants.json" t = Just <$> maybe (generateConstantsJSON f) (const $ return False) t
generateWebFile f@"constants.js" t = Just <$> maybe (generateConstantsJS f) (const $ return False) t
generateWebFile f@"routes.js" t = Just <$> maybe (generateRoutesJS f) (const $ return False) t
generateWebFile f@"messages.js" t = Just <$> generateMessagesJS t f
generateWebFile f@"templates.js" t = Just <$> generateTemplatesJS t f
generateWebFile f t
  | (b, e) <- splitExtensions f, e `elem` [".js", ".min.js"]
  , Just p <- lookup b jsLibs = webLinkFile p f t
  | otherwise = webLinkFile "web" f t

allWebFiles :: [RawFilePath]
allWebFiles =
  [ "constants.json", "constants.js", "routes.js", "messages.js" {-, "templates.js"-}
  ]

generateWebFiles :: IO [RawFilePath]
generateWebFiles = do
  wd <- webDataFiles
  forM (wd ++ allWebFiles) $ \f -> do
    _ <- removeFile (webDir </> f)
    Just _ <- generateWebFile f Nothing
    return f
