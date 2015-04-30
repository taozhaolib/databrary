{-# LANGUAGE TypeOperators #-}
module Databrary.Model.Id 
  ( module Databrary.Model.Id.Types
  , idIso
  , pathId
  ) where

import qualified Databrary.Iso as I
import Databrary.HTTP.Route.Path
import Databrary.HTTP.Route.PathParser
import Databrary.Model.Kind
import Databrary.Model.Id.Types

idIso :: IdType a I.<-> Id a
idIso = Id I.:<->: unId

pathId :: forall a . (PathDynamic (IdType a), Kinded a) => PathParser (Id a)
pathId = PathFixed (kindOf (undefined :: a)) >/> idIso I.<$> PathDynamic
