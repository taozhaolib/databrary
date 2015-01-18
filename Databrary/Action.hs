{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification, DefaultSignatures, TypeFamilies #-}
module Databrary.Action
  ( RequestM(..)
  , respond
  , SomeAction
  , action
  , routeApp
  ) where

import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai

import Databrary.Web.Route (RouteM, routeRequest)
import Databrary.Action.Types
import Databrary.Action.Wai
import Databrary.Action.App
import Databrary.Resource

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
