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
  , redirectRouteResponse
  , forbiddenResponse
  , notFoundResponse
  , okResponse
  , result
  , guardAction
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
import qualified Data.ByteString.Char8 as BSC
import Data.Functor.Contravariant (Contravariant(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as T
import Network.HTTP.Types (Method, methodGet, StdMethod(..), renderStdMethod, encodePathSegments, Status, ok200, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
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

redirectRouteResponse :: ActionM q m => ResponseHeaders -> RouteAction qa -> m Response
redirectRouteResponse h RouteAction{ actionMethod = g, actionRoute = r }
  | g == methodGet = emptyResponse seeOther303 ((hLocation, r) : h) -- XXX absolute URL
  | otherwise = fail ("redirectRouteResponse: " ++ BSC.unpack g ++ " " ++ BSC.unpack r)

forbiddenResponse :: ActionM q m => m Response
forbiddenResponse = emptyResponse forbidden403 []

notFoundResponse :: ActionM q m => m Response
notFoundResponse = emptyResponse notFound404 []

okResponse :: (ActionM q m, ResponseData r) => ResponseHeaders -> r -> m Response
okResponse = returnResponse ok200

guardAction :: (ActionM q m, MonadIO m) => Bool -> m Response -> m ()
guardAction True _ = return ()
guardAction False r = result =<< r

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
runAppRoute route rc = runWai $ withApp rc $
  fromMaybe notFoundResponse . R.routeRequest route =<< peek
