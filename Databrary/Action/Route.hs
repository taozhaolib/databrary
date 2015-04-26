{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , RouteAction(..)
  , actionURL
  , action
  , multipartAction
  , R.toRoute
  , API(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (withReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Functor (($>))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Monoid ((<>))
import Network.HTTP.Types (Method, methodGet, StdMethod(..), renderStdMethod, encodePathSegments)

import Databrary.HTTP.Request
import Databrary.Action.Types
import qualified Databrary.HTTP.Route as R

data RouteAction q = RouteAction 
  { actionMethod :: !Method
  , actionMultipart :: !Bool
  , actionRoute :: BS.ByteString
  , routeAction :: Action q
  }

actionURL :: RouteAction q -> Maybe Request -> BS.ByteString
actionURL RouteAction{ actionMethod = g, actionRoute = r } req
  | g == methodGet = maybe id ((<>) . requestHost) req r
  | otherwise = error ("actionURL: " ++ BSC.unpack g ++ " " ++ BSC.unpack r)

action :: R.Routable r => StdMethod -> r -> Action q -> RouteAction q
action meth r act = RouteAction
  { actionMethod = renderStdMethod meth
  , actionMultipart = False
  , actionRoute = eps $ R.toRoute r
  , routeAction = act
  } where
  eps [] = "/"
  eps p = BSL.toStrict $ BSB.toLazyByteString $ encodePathSegments p

multipartAction :: RouteAction q -> RouteAction q
multipartAction a = a{ actionMultipart = True }

mapRouteAction :: (Action q -> Action q') -> RouteAction q -> RouteAction q'
mapRouteAction f (RouteAction m p r a) = RouteAction m p r (f a)

instance Contravariant RouteAction where
  contramap f = mapRouteAction (withReaderT f)

data API
  = HTML
  | JSON
  deriving (Eq)

instance R.Routable API where
  route = R.fixed "api" $> JSON <|> return HTML
  toRoute HTML = []
  toRoute JSON = ["api"]
