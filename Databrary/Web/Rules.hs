{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Applicative ((<*>))
import Control.Monad (when, mzero, msum)
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

fixedGenerators :: [(FilePath, WebGenerator)]
fixedGenerators =
  [ ("constants.json", generateConstantsJSON)
  , ("constants.js",   generateConstantsJS)
  , ("routes.js",      generateRoutesJS)
  , ("messages.js",    generateMessagesJS)
  , ("templates.js",   generateTemplatesJS)
  , ("app.min.js",     generateUglifyJS)
  , ("app.min.css",    generateStylusCSS)
  ]

generateFixed :: WebGenerator
generateFixed fo@(f, _)
  | Just g <- lookup (webFileRel f) fixedGenerators = g fo
  | otherwise = mzero

generateStatic :: WebGenerator
generateStatic fo@(f, _) = fileNewer f fo

generateRules :: WebGenerator
generateRules f = msum $ map ($ f)
  [ generateFixed
  , generateCoffeeJS
  , generateLib
  , generateStatic
  ]

updateWebInfo :: (WebFilePath, Maybe WebFileInfo) -> WebGeneratorM WebFileInfo
updateWebInfo (f, o) = do
  n <- liftIO $ makeWebFileInfo f
  case o of
    Just i@WebFileInfo{ webFileHash = oh, webFileTimestamp = ot } | oh == webFileHash n -> do
      liftIO $ when (ot /= webFileTimestamp n) $
        setFileTimestamps f ot ot
      return i
    _ -> do
      modify $ HM.insert f n
      return n

generateWebFile :: WebFilePath -> WebGeneratorM WebFileInfo
generateWebFile f = do
  o <- gets $ HM.lookup f
  let fo = (f, o)
  r <- generateRules fo `catchError` (throwError . label (webFileRel f))
  if r then updateWebInfo fo else maybe (throwError (webFileRel f)) return o
  where
  label n "" = n
  label n s = n ++ ": " ++ s

generateAll :: WebGeneratorM ()
generateAll = do
  mapM_ generateWebFile $
    allWebLibs False ++
    map (fromFilePath . fst) fixedGenerators

generateWebFiles :: IO WebFileMap
generateWebFiles = do
  either fail return
    =<< runExceptT . execStateT generateAll . HM.fromList 
    =<< mapM (\f -> (f, ) <$> makeWebFileInfo f) =<< allWebFiles
