{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}
module Databrary.Action
  ( ActionM
  , Action
  , BAction
  , Result
  , respond
  , runAction
  , SomeAction(..)
  , action
  , routeAction
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Control.Monad.State (modify)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (Monoid, mappend)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (notFound404)

import Databrary.App
import Databrary.Web.Wai
import Databrary.Web.Route (RouteM, routeRequest)

type ActionM r = WaiT r AppM
type Action r = ActionM r Status
type BAction = Action Blaze.Builder

class Result r => Respond r b where
  respond :: b -> ActionM r ()

instance (Result r, Monoid r) => Respond r r where respond = modify . mappend
instance Respond Blaze.Builder T.Text where respond = respond . Blaze.fromText
instance Respond Blaze.Builder TL.Text where respond = respond . Blaze.fromLazyText
instance Respond Blaze.Builder BS.ByteString where respond = respond . Blaze.fromByteString
instance Respond Blaze.Builder BSL.ByteString where respond = respond . Blaze.fromLazyByteString

runAction :: Result r => Action r -> WaiApplication AppM
runAction = runWaiT

defaultAction :: Action Blaze.Builder
defaultAction = return notFound404

data SomeAction = forall r . Result r => SomeAction (Action r)

action :: Result r => Action r -> RouteM SomeAction
action = return . SomeAction

routeAction :: RouteM SomeAction -> WaiApplication AppM
routeAction route request send =
  case routeRequest route request of
    Just (SomeAction r) -> runAction r request send
    Nothing -> runAction defaultAction request send
