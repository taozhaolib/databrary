module Constants (
    Constants(..)
  , constants
  , permission
  , classification
  ) where

import Data.StrMap (lookup)
import Data.Maybe (Maybe(..))
import Util

type Constants = {
    permission :: [String]
  , classification :: [String]
  , consent :: [String]
  }

foreign import constants :: Constants

permissionMap = invertArray constants.permission
classificationMap = invertArray constants.classification

permission :: String -> Number
permission = unsafeLookup permissionMap

classification :: String -> Number
classification = unsafeLookup classificationMap
