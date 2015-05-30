{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( allWebJS
  , generateUglifyJS
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import qualified System.FilePath as FP
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files

allWebJS :: IO [WebFilePath]
allWebJS = ("app.js" :) . filter (not . (liftM2 (||) (isPrefixOf "lib/") (`elem` ["app.js", "debug.js"])) . webFileRel) <$> findWebFiles ".js"

generateUglifyJS :: WebGenerator
generateUglifyJS f = do
  jl <- liftIO allWebJS
  guard (not $ null jl)
  webRegenerate (do
    let fm = f <.> ".map"
    callProcess (binDir FP.</> "uglifyjs") $ ["--output", webFileAbs f, "--source-map", webFileAbs fm, "--source-map-url", webFileRel fm, "--prefix", "relative", "--screw-ie8", "--mangle", "--compress", "--define", "DEBUG=false"]
      ++ map webFileAbs jl)
    [] jl f
