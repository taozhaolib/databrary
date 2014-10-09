module Main where

import Angular.Module
import Angular.DI (annotate)

import Constants (Constants(..))

foreign import app :: Module

main = do
  directive "classificationSelect" (annotate ClassificationSelect.directive) app
