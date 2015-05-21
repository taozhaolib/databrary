{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import System.Posix.FilePath (replaceExtension)

import Databrary.Web.Types
import Databrary.Web.Files

generateCoffeeJS :: WebGenerator
generateCoffeeJS f t = do
  s <- lift $ webDataFile $ replaceExtension f ".coffee"
  mzero
