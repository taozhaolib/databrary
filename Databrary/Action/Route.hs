{-# LANGUAGE ExistentialQuantification, DefaultSignatures, TypeFamilies, RankNTypes #-}
module Databrary.Action.Route
  ( RouteAction(..)
  , action
  , runRoute
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import Data.Functor.Contravariant (Contravariant(..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (Method, StdMethod, renderStdMethod, encodePathSegments)

import Control.Has (peek)
import qualified Databrary.Web.Route as R
import Databrary.Action.Types

data RouteAction q = RouteAction 
  { actionMethod :: Method
  , actionRoute :: BS.ByteString
  , routeAction :: Action q
  }

action :: StdMethod -> [T.Text] -> Action q -> RouteAction q
action meth path act = RouteAction
  { actionMethod = renderStdMethod meth
  , actionRoute = Blaze.toByteString $ encodePathSegments path
  , routeAction = act
  }

mapRouteAction :: (Action q -> Action q') -> RouteAction q -> RouteAction q'
mapRouteAction f (RouteAction m r a) = RouteAction m r (f a)

instance Contravariant RouteAction where
  contramap f = mapRouteAction (withAction f)

runRoute :: ActionData q => R.RouteM (Action q) -> Action q
runRoute route =
  fromMaybe notFoundResponse . R.routeRequest route =<< peek
