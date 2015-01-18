{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification, DefaultSignatures #-}
module Databrary.Action.Types
  ( HasRequest(..)
  , RequestM(..)
  , ActionT
  , ActionM
  , Action
  , BAction
  , withAction
  , notFound
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad (liftM)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.RWS.Strict (RWST, withRWST)
import qualified Data.ByteString as BS
import Data.Monoid (Monoid)
import Network.HTTP.Types (ResponseHeaders, HeaderName, Status, notFound404)
import Network.Wai (Request, requestHeaders)

class HasRequest r where
  toRequest :: r -> Request

instance HasRequest Request where
  toRequest = id

class Monad m => RequestM m where
  getRequest :: m Request
  default getRequest :: (MonadReader r m, HasRequest r) => m Request
  getRequest = asks toRequest
  getRequestHeader :: HeaderName -> m (Maybe BS.ByteString)
  getRequestHeader h = liftM (lookup h . requestHeaders) getRequest

instance (Monad m, HasRequest r) => RequestM (ReaderT r m)
instance (Monad m, Monoid w, HasRequest r) => RequestM (RWST r w s m)

type ActionT q = RWST q ResponseHeaders
type ActionM q r = ActionT q r IO
type Action q r = ActionM q r Status
type BAction q = Action q Blaze.Builder

withAction :: (q -> q') -> ActionT q' r m a -> ActionT q r m a
withAction f = withRWST $ (,) . f

notFound :: BAction q
notFound = return notFound404
