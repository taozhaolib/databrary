{-# LANGUAGE ExistentialQuantification, RecordWildCards, ImpredicativeTypes #-}
module Databrary.HTTP.Route
  ( Route(..)
  , RouteMap
  , routeMap
  , lookupRoute
  , renderRoute
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Network.HTTP.Types (Method)
import qualified Network.Wai as Wai

import Databrary.HTTP.Route.Path
import qualified Databrary.HTTP.Route.PathMap as PM
import Databrary.HTTP.Route.PathParser

data Route a r = Route
  { routeMethod :: !Method
  , routeMultipart :: !Bool
  , routePath :: PathParser a
  , routeAction :: a -> r
  }

instance Functor (Route a) where
  fmap f r = r{ routeAction = fmap f $ routeAction r }

data AnyRoute r = forall a . AnyRoute (Route a r)

type RouteMap r = HM.HashMap Method (PM.PathMap (AnyRoute r))

empty :: RouteMap a
empty = HM.empty

insert :: Method -> [PathElement] -> AnyRoute r -> RouteMap r -> RouteMap r
insert a p r m = HM.insertWith PM.union a (PM.singleton p r) m

insertRoute :: (forall a . Route a r) -> RouteMap r -> RouteMap r
insertRoute r@Route{ routeMethod = a, routePath = p } m =
  foldl' (\m' p' -> insert a p' (AnyRoute r) m') m $ pathElements {- =<< pathParserExpand -} p

routeMap :: [forall a . Route a r] -> RouteMap r
routeMap = foldl' (flip insertRoute) empty

lookupRoute :: Wai.Request -> RouteMap r -> Maybe r
lookupRoute q = r <=< PM.lookup p <=< HM.lookup (Wai.requestMethod q) where
  r (AnyRoute Route{..}) = routeAction <$> pathParser routePath p
  p = Wai.pathInfo q

renderRoute :: Route a r -> a -> BSB.Builder
renderRoute Route{ routePath = p } a = renderPath $ pathGenerate p a
