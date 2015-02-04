{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action
  ( Request
  , Action
  , ActionM
  , AppAction
  , AuthAction
  , getRequestHeader
  , getRequestHeaders

  , Response
  , returnResponse
  , emptyResponse
  , notFoundResponse
  , okResponse
  , result
  , maybeAction

  , StdMethod(GET, POST)
  , RouteAction(..)
  , AppRAction
  , AuthRAction
  , action
  , withAuth
  , R.toRoute
  , apiRoute

  , runAppRoute
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import Data.Functor.Contravariant (Contravariant(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as T
import Network.HTTP.Types (Method, StdMethod(..), renderStdMethod, encodePathSegments, Status, ok200, notFound404, ResponseHeaders)
import qualified Network.Wai as Wai

import Control.Has (peek)
import Databrary.Action.Types
import Databrary.Action.Request
import Databrary.Action.Response
import Databrary.Action.App
import Databrary.Action.Auth
import Databrary.Resource
import qualified Databrary.Web.Route as R

emptyResponse :: ActionM q m => Status -> ResponseHeaders -> m Response
emptyResponse s h = returnResponse s h (mempty :: Blaze.Builder)

notFoundResponse :: ActionM q m => m Response
notFoundResponse = emptyResponse notFound404 []

okResponse :: (ActionM q m, ResponseData r) => ResponseHeaders -> r -> m Response
okResponse = returnResponse ok200

maybeAction :: (ActionM q m, MonadIO m) => Maybe a -> m a
maybeAction (Just a) = return a
maybeAction Nothing = result =<< notFoundResponse

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

apiRoute :: Bool -> [T.Text] -> [T.Text]
apiRoute False = id
apiRoute True = ("api" :)

mapRouteAction :: (Action q -> Action q') -> RouteAction q -> RouteAction q'
mapRouteAction f (RouteAction m r a) = RouteAction m r (f a)

instance Contravariant RouteAction where
  contramap f = mapRouteAction (withAction f)

type AppRAction = RouteAction AppRequest
type AuthRAction = RouteAction AuthRequest

runAppRoute :: R.RouteM AppAction -> Resource -> Wai.Application
runAppRoute route rc = runWai $ runApp rc $
  fromMaybe notFoundResponse . R.routeRequest route =<< peek
