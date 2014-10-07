module Main where

import Control.Monad (when)
import Control.Monad.Eff
import Angular.Module
import Angular.DI (annotate, RootScope(..))

foreign import app :: Module

f (RootScope r) = do
  _ <- Debug.Trace.trace $ if (Angular.Scope.root r `refEq` r) then "good" else "bad"
  return 0

main = do
  service "testPurs" (annotate f) app
