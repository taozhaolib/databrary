{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( allWebJS
  , generateUglifyJS
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard, liftM2)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
import qualified System.FilePath as FP
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Web
import Databrary.Web.Files

allWebJS :: IO [WebFilePath]
allWebJS = ("app.js" :) . filter (not . (liftM2 (||) (isPrefixOf "lib/") (`elem` ["app.js", "debug.js"])) . webFileRel) <$> findWebFiles ".js"

generateUglifyJS :: WebGenerator
generateUglifyJS f t = do
  jl <- lift allWebJS
  guard (not $ null jl)
  jt <- mapM webFileTime jl
  lift $ webRegenerate (maximum jt) f t $ do
    let fm = f <.> ".map"
    callProcess (binDir FP.</> "uglifyjs") $ ["--output", webFileAbs f, "--source-map", webFileAbs fm, "--source-map-url", webFileRel fm, "--prefix", "relative", "--screw-ie8", "--mangle", "--compress", "--define", "DEBUG=false"]
      ++ map webFileAbs jl
