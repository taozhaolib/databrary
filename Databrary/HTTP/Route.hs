{-# LANGUAGE ExistentialQuantification #-}
module Databrary.HTTP.Route
  ( Route(..)
  , AnyRoute(..)
  , empty
  ) where

import Prelude hiding (lookup)

import Control.Monad ((<=<))
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Network.HTTP.Types (Method)
import qualified Network.Wai as Wai

import Databrary.HTTP.Route.Path
import qualified Databrary.HTTP.Route.PathMap as PM
import Databrary.HTTP.Route.PathParser

data Route a r = Route
  { routeMethod :: !Method
  , routePath :: PathParser a
  , routeAction :: a -> r
  }

instance Functor (Route a) where
  fmap f r = r{ routeAction = fmap f $ routeAction r }

data AnyRoute r = forall a . AnyRoute (Route a r)

type RouteMap r = HM.HashMap Method (PM.PathMap (AnyRoute r))

empty :: RouteMap a
empty = HM.empty

lookup :: Wai.Request -> RouteMap r -> Maybe (AnyRoute r)
lookup q = PM.lookup (Wai.pathInfo q) <=< HM.lookup (Wai.requestMethod q) 

insert :: Method -> [PathElement] -> AnyRoute r -> RouteMap r -> RouteMap r
insert a p r m = HM.insertWith PM.union a (PM.singleton p r) m

insertRoute :: Route a r -> RouteMap r -> RouteMap r
insertRoute r@(Route a p _) m =
  foldl' (\m' p' -> insert a p' (AnyRoute r) m') m $ pathElements {- =<< pathParserExpand -} p
