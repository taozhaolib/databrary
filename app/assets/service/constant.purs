module Constants (
    Constants(..)
  , constants
  , permission
  , unsafePermission
  ) where

import Data.Maybe (Maybe(..))
import Map

type Constants = {
    permission :: [String]
  , classification :: [String]
  , consent :: [String]
  }

foreign import constants :: Constants

permissionMap = invertArray constants.permission

permission :: String -> Maybe Number
permission = lookup permissionMap

unsafePermission :: String -> Number
unsafePermission = unsafeLookup permissionMap
