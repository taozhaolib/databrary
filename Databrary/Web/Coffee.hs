{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Web
import Databrary.Web.Files

generateCoffeeJS :: WebGenerator
generateCoffeeJS f t
  | (b, e) <- splitWebFileExtensions f, e `elem` [".js", ".js.map"] = do
    let src = b <.> ".coffee"
    wt <- webFileTime src
    lift $ webRegenerate wt f t $
      callProcess (binDir </> "coffee") ["-b", "-c", "-m", "-o", takeDirectory $ webFileAbs f, webFileAbs src]
  | otherwise = mzero
