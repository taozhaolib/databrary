{-# LANGUAGE ExistentialQuantification, DefaultSignatures, TypeFamilies #-}
module Databrary.Action.Route
  ( RouteAction
  , actionMethod
  , actionRoute
  , action
  , toRoute
  , routeApp
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.HTTP.Types (Method, StdMethod, renderStdMethod, encodePathSegments)
import qualified Network.Wai as Wai

import Databrary.Web.Route (RouteM, routeRequest, toRoute)
import Databrary.Action.Types
import Databrary.Action.Wai
import Databrary.Action.App
import Databrary.Action.Response
import Databrary.Resource

data RouteAction q = forall r . Response r => RouteAction 
  { actionMethod :: Method
  , actionRoute :: BS.ByteString
  , routeAction :: Action q r
  }

action :: Response r => StdMethod -> [T.Text] -> Action q r -> RouteAction q
action meth path act = RouteAction
  { actionMethod = renderStdMethod meth
  , actionRoute = Blaze.toByteString $ encodePathSegments path
  , routeAction = act
  }

withRouteAction :: (q -> q') -> RouteAction q' -> RouteAction q
withRouteAction f (RouteAction m r a) = RouteAction m r $ withAction f a

routeWai :: RouteM (RouteAction Wai.Request) -> Wai.Application
routeWai route request send = 
  case routeRequest route request of
    Just (RouteAction _ _ w) -> runWai w request send
    Nothing -> runWai notFoundResult request send

routeApp :: Resource -> RouteM (RouteAction AppRequest) -> Wai.Application
routeApp rc route request send = do
  ts <- getCurrentTime
  routeWai (fmap (withRouteAction (\rq -> AppRequest rc rq ts)) route) request send
