{-# LANGUAGE ScopedTypeVariables #-}
module Databrary.Model.Id 
  ( module Databrary.Model.Types.Id
  , pathIdArg
  ) where

import Snap.Core (MonadSnap, dir, pathArg)

import Databrary.Model.Types.Id

pathIdArg :: forall a b m . (MonadSnap m, HasId a) => (Id a -> m b) -> m b
pathIdArg f = dir (kindOf (undefined :: a)) $ pathArg f
