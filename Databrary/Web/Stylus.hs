{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad (mzero)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files

generateStylusCSS :: WebGenerator
generateStylusCSS f
  | (b, e) <- splitWebFileExtensions f, e `elem` [".css", ".min.css"] = do
    let src = b <.> ".styl"
    webRegenerate
      (callProcess (binDir </> "stylus") $ (if e == ".min.css" then ("-c":) else id) ["-u", "nib", "-o", webFileAbs f, webFileAbs src])
      [] [src] f
  | otherwise = mzero
