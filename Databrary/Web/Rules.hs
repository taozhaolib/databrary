{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Applicative ((<*>))
import Control.Monad (mzero, msum)
import Control.Monad.Except (runExceptT, catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (execStateT, modify, gets)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)

import Databrary.Ops
import Databrary.Files
import Databrary.Model.Format
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

staticFormats :: [(String, BS.ByteString)]
staticFormats = concatMap (\f -> map (\e -> ('.':BSC.unpack e, formatMimeType f)) $ formatExtension f) allFormats ++
  [ (".html", "text/html")
  , (".js", "application/javascript")
  , (".css", "text/css")
  , (".svg", "image/svg+xml")
  ]

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = WebFileInfo
  (fromMaybe "application/octet-stream" $ lookup (takeExtension $ webFileRel f) staticFormats)
  <$> hashFile (webFileAbsRaw f)
  <*> (modificationTimestamp <$> getFileStatus f)

staticGenerators :: [(FilePath, WebGenerator)]
staticGenerators =
  [ ("constants.json", generateConstantsJSON)
  , ("constants.js",   generateConstantsJS)
  , ("routes.js",      generateRoutesJS)
  ]

fixedGenerators :: [(FilePath, WebGenerator)]
fixedGenerators =
  [ ("messages.js",    generateMessagesJS)
  , ("templates.js",   generateTemplatesJS)
  , ("app.min.js",     generateUglifyJS)
  , ("app.min.css",    generateStylusCSS)
  ]

generateFixed :: Bool -> WebGenerator
generateFixed a fo@(f, _)
  | Just g <- lookup (webFileRel f) $ (if a then (staticGenerators ++) else id) fixedGenerators = g fo
  | otherwise = mzero

generateStatic :: WebGenerator
generateStatic fo@(f, _) = fileNewer f fo

generateRules :: Bool -> WebGenerator
generateRules a f = msum $ map ($ f)
  [ generateFixed a
  , generateCoffeeJS
  , generateLib
  , generateStatic
  ]

updateWebInfo :: WebFilePath -> WebGeneratorM WebFileInfo
updateWebInfo f = do
  n <- liftIO $ makeWebFileInfo f
  modify $ HM.insert f n
  return n

generateWebFile :: Bool -> WebFilePath -> WebGeneratorM WebFileInfo
generateWebFile a f = catchError (do
  o <- gets $ HM.lookup f
  r <- generateRules a (f, o)
  if r then updateWebInfo f else maybe (throwError (webFileRel f)) return o)
  (throwError . label (webFileRel f))
  where
  label n "" = n
  label n s = n ++ ": " ++ s

generateAll :: WebGeneratorM ()
generateAll = do
  mapM_ (generateWebFile True) $
    allWebLibs False ++
    map (fromFilePath . fst) (staticGenerators ++ fixedGenerators)

generateWebFiles :: IO WebFileMap
generateWebFiles = do
  either fail return
    =<< runExceptT . execStateT generateAll . HM.fromList 
    =<< mapM (\f -> (f, ) <$> makeWebFileInfo f) =<< allWebFiles
