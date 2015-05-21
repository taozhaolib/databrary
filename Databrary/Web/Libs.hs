{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  ) where

import Control.Monad (mzero)
import System.Posix.FilePath (splitExtensions)

import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files

jsLibs :: [(RawFilePath, FilePath)]
jsLibs =
  [ ("jquery",              "bower_components/jquery/dist")
  , ("angular",             "bower_components/angular")
  , ("angular-route",       "bower_components/angular-route")
  , ("ng-flow-standalone",  "bower_components/ng-flow/dist")
  , ("lodash",              "bower_components/lodash")
  ]

generateLib :: WebGenerator
generateLib f t
  | (b, e) <- splitExtensions f, e `elem` [".js", ".min.js"]
  , Just p <- lookup b jsLibs = webLinkFile p f t
  | otherwise = mzero
