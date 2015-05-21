{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import System.FilePath ((</>))
import System.Posix.FilePath (splitExtensions, (<.>), takeDirectory)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files

generateCoffeeJS :: WebGenerator
generateCoffeeJS f t 
  | (b, e) <- splitExtensions f, e `elem` [".js", ".js.map"] = do
    src <- lift $ webDataFile $ b <.> ".coffee"
    (_, wt) <- MaybeT $ fileInfo src
    lift $ webRegenerate wt f t $ \dst ->
      callProcess (binDir </> "coffee") ["-b", "-c", "-m", "-o", unRawFilePath $ takeDirectory dst, unRawFilePath src]
  | otherwise = mzero
