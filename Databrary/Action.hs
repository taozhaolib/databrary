{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification, DefaultSignatures, TypeFamilies #-}
module Databrary.Action
  ( getRequest
  , getRequestHeader
  , HasResource(..)
  , respond
  , SomeAction
  , action
  , routeApp
  ) where

import Control.Monad (liftM)
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (HeaderName)
import qualified Network.Wai as Wai

import Databrary.Web.Route (RouteM, routeRequest)
import Databrary.Action.Types
import Databrary.Action.Wai
import Databrary.Action.App
import Databrary.Resource

class Monad m => HasRequest m where
  getRequest :: m Wai.Request
  default getRequest :: (MonadTrans t, HasRequest b, m ~ t b) => t b Wai.Request
  getRequest = lift getRequest
  getRequestHeader :: HeaderName -> m (Maybe BS.ByteString)
  getRequestHeader h = liftM (lookup h . Wai.requestHeaders) getRequest

instance Monad m => HasRequest (WaiT r m) where getRequest = ask
instance Monad m => HasRequest (AppT r m) where getRequest = asks appRequest

class Monad m => HasResource m where
  getResource :: (Resource -> a) -> m a
  default getResource :: (MonadTrans t, HasResource b, m ~ t b) => (Resource -> a) -> t b a
  getResource = lift . getResource

instance Monad m => HasResource (ResourceT m) where getResource = asks
instance Monad m => HasResource (AppT r m) where getResource f = asks (f . appResource)

data SomeAction q = forall r . Result r => SomeAction (Action q r)

action :: Result r => Action q r -> RouteM (SomeAction q)
action = return . SomeAction

withSomeAction :: (q -> q') -> SomeAction q' -> SomeAction q
withSomeAction f (SomeAction a) = SomeAction $ withAction f a

routeWai :: RouteM (SomeAction Wai.Request) -> Wai.Application
routeWai route request send = 
  case fromMaybe (SomeAction notFound) (routeRequest route request) of
    SomeAction w -> runWai w request send

routeApp :: Resource -> RouteM (SomeAction AppRequest) -> Wai.Application
routeApp rc = routeWai . fmap (withSomeAction (initApp rc))
