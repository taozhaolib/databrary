module Constants (
    Constants(..)
  ) where

import Angular.DI (Dependency, Service)

newtype Constants = Constants {
    permission :: [String]
  , classification :: [String]
  , consent :: [String]
  }

instance dependencyConstant :: Dependency Constants where
  name = "constantService"
instance serviceConstant :: Service Constants

