{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import System.FilePath (takeDirectory)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files

generateCoffeeJS :: WebGenerator
generateCoffeeJS f
  | (b, e) <- splitWebFileExtensions f, e `elem` [".js", ".js.map"] = do
    let src = b <.> ".coffee"
    webRegenerate
      (callProcess (binDir </> "coffee") ["-b", "-c", "-m", "-o", takeDirectory (webFileAbs f), webFileAbs src])
      [] [src] f
  | otherwise = mzero
