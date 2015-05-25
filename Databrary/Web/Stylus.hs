{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import System.FilePath ((</>))
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Web
import Databrary.Web.Files

generateStylusCSS :: WebGenerator
generateStylusCSS f t 
  | (b, e) <- splitWebFileExtensions f, e `elem` [".css", ".min.css"] = do
    let src = b <.> ".styl"
    wt <- webFileTime src
    lift $ webRegenerate wt f t $
      callProcess (binDir </> "stylus") $ (if e == ".min.css" then ("-c":) else id) ["-u", "nib", "-o", webFileAbs f, webFileAbs src]
  | otherwise = mzero
