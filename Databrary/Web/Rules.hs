{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import qualified Data.Traversable as Trav
import System.Posix.FilePath ((</>))
import System.Posix.Files.ByteString (createLink)

import Databrary.Store
import Databrary.Model.Time
import Databrary.Web.Files
import Databrary.Web.Constants
import Databrary.Web.Routes
import Databrary.Web.Templates

generateWebFile :: RawFilePath -> Maybe Timestamp -> IO (Maybe Bool)
generateWebFile f@"constants.json" t = Just <$> maybe (generateConstantsJSON f) (const $ return False) t
generateWebFile f@"constants.js" t = Just <$> maybe (generateConstantsJS f) (const $ return False) t
generateWebFile f@"routes.js" t = Just <$> maybe (generateRoutesJS f) (const $ return False) t
generateWebFile f@"templates.js" t = Just <$> generateTemplatesJS t f
generateWebFile f t = do
  wf <- webDataFile f
  wi <- fileInfo wf
  Trav.forM wi $ \(_, wt) ->
    webRegenerate wt f t $ createLink wf

allWebFiles :: [RawFilePath]
allWebFiles = ["constants.json", "constants.js", "routes.js" {-, "templates.js"-}]

generateWebFiles :: IO [RawFilePath]
generateWebFiles = do
  wd <- webDataFiles
  forM (wd ++ allWebFiles) $ \f -> do
    _ <- removeFile (webDir </> f)
    Just _ <- generateWebFile f Nothing
    return f
