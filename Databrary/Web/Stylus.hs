{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import System.FilePath ((</>))
import System.Posix.FilePath (splitExtensions, (<.>))
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files

generateStylusCSS :: WebGenerator
generateStylusCSS f t 
  | (b, e) <- splitExtensions f, e `elem` [".css", ".min.css"] = do
    src <- lift $ webDataFile $ b <.> ".styl"
    (_, wt) <- MaybeT $ fileInfo src
    lift $ webRegenerate wt f t $ \dst ->
      callProcess (binDir </> "stylus") $ (if e == ".min.css" then ("-c":) else id) ["-u", "nib", "-o", unRawFilePath dst, unRawFilePath src]
  | otherwise = mzero
