{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , allLibs
  ) where

import Control.Monad (mzero)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>), splitExtensions)

import Databrary.Web.Files hiding ((<.>))

jsLibs :: [(FilePath, FilePath)]
jsLibs =
  [ ("jquery",              "bower_components/jquery/dist")
  , ("angular",             "bower_components/angular")
  , ("angular-route",       "bower_components/angular-route")
  , ("ng-flow-standalone",  "bower_components/ng-flow/dist")
  , ("lodash",              "bower_components/lodash")
  ]

generateLib :: WebGenerator
generateLib f t
  | ("lib/", l) <- splitFileName (webFileRel f)
  , (b, e) <- splitExtensions l
  , e `elem` [".js", ".min.js"]
  , Just p <- lookup b jsLibs = webLinkFile (p </> l) f t
  | otherwise = mzero

allLibs :: [WebFilePath]
allLibs = map (fromString . ("lib" </>) . (<.> ".min.js") . fst) jsLibs
