module Databrary.Kind
  ( Kinded(..)
  ) where

import Data.String (IsString)

class Kinded a where
  kindOf :: IsString s => a -> s
