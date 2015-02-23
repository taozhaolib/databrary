{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , RouteAction(..)
  , actionURL
  , action
  , R.toRoute
  , apiRoute
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad.Reader (withReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Functor.Contravariant (Contravariant(..))
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types (Method, methodGet, StdMethod(..), renderStdMethod, encodePathSegments)

import Databrary.Web.Request
import Databrary.Action.Types
import qualified Databrary.Web.Route as R

data RouteAction q = RouteAction 
  { actionMethod :: Method
  , actionRoute :: BS.ByteString
  , routeAction :: Action q
  }

actionURL :: RouteAction q -> Request -> BS.ByteString
actionURL RouteAction{ actionMethod = g, actionRoute = r } req
  | g == methodGet = requestHost req <> r
  | otherwise = error ("actionURL: " ++ BSC.unpack g ++ " " ++ BSC.unpack r)

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
  contramap f = mapRouteAction (withReaderT f)
