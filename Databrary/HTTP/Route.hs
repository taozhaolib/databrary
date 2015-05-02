{-# LANGUAGE ExistentialQuantification, RecordWildCards, ImpredicativeTypes #-}
module Databrary.HTTP.Route
  ( Route(..)
  , RouteMap
  , routeMap
  , lookupRoute
  , routeURL
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Monoid ((<>))
import Network.HTTP.Types (Method)
import qualified Network.Wai as Wai

import Databrary.Iso.Prim (Invariant(..))
import Databrary.HTTP.Request
import Databrary.HTTP.Route.Path
import qualified Databrary.HTTP.Route.PathMap as PM
import Databrary.HTTP.Route.PathParser

data Route r a = Route
  { routeMethod :: !Method
  , routeMultipart :: !Bool
  , routePath :: PathParser a
  , routeAction :: a -> r
  }

instance Invariant (Route r) where
  invMap f g r = r
    { routePath = invMap f g $ routePath r
    , routeAction = routeAction r . g
    }

data AnyRoute r = forall a . AnyRoute (Route r a)

type RouteMap r = HM.HashMap Method (PM.PathMap (AnyRoute r))

empty :: RouteMap a
empty = HM.empty

insert :: Method -> [PathElement] -> AnyRoute r -> RouteMap r -> RouteMap r
insert a p r m = HM.insertWith PM.union a (PM.singleton p r) m

insertRoute :: (forall a . Route r a) -> RouteMap r -> RouteMap r
insertRoute r@Route{ routeMethod = a, routePath = p } m =
  foldl' (\m' p' -> insert a p' (AnyRoute r) m') m $ pathElements {- =<< pathParserExpand -} p

routeMap :: [forall a . Route r a] -> RouteMap r
routeMap = foldl' (flip insertRoute) empty

lookupRoute :: Wai.Request -> RouteMap r -> Maybe r
lookupRoute q = r <=< PM.lookup p <=< HM.lookup (Wai.requestMethod q) where
  r (AnyRoute Route{..}) = routeAction <$> pathParser routePath p
  p = Wai.pathInfo q

routeURL :: Maybe Wai.Request -> Route r a -> a -> BSB.Builder
routeURL req Route{ routePath = p } a =
  maybe id ((<>) . BSB.byteString . requestHost) req $ renderPath $ pathGenerate p a
