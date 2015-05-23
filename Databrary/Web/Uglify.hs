{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( generateUglifyJS
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard, liftM2)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
import qualified System.FilePath as FP
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Web.Files

generateUglifyJS :: WebGenerator
generateUglifyJS f t = do
  jl <- lift $ ("app.js" :) . filter (not . (liftM2 (||) (isPrefixOf "lib/") (`elem` ["app.js", "debug.js"])) . webFileRel) <$> findWebFiles ".js"
  guard (not $ null jl)
  jt <- mapM webFileTime jl
  lift $ webRegenerate (maximum jt) f t $ \wf -> do
    let wfm = wf <.> ".map"
    callProcess (binDir FP.</> "uglifyjs") $ ["--output", webFileAbs wf, "--source-map", webFileAbs wfm, "--source-map-url", webFileRel wfm, "--prefix", "relative", "--screw-ie8", "--mangle", "--compress", "--define", "DEBUG=false"]
      ++ map webFileAbs jl
