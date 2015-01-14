{-# LANGUAGE ScopedTypeVariables #-}
module Databrary.Model.Id 
  ( module Databrary.Model.Types.Id
  , routeId
  ) where

import Control.Applicative ((<$>))
import Databrary.Model.Types.Id
import qualified Data.Text.Read as T
import qualified Databrary.Web.Route as R

routeId :: HasId a => R.RouteM (Id a)
routeId = Id <$> R.reader T.decimal
