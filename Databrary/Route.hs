{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Databrary.Route
  ( RouteM

  , method
  , on, StdMethod(..)

  , text
  , fixed
  , path
  , read
  , reader
  , Routable(..)

  , routeRequest
  ) where

import Prelude hiding (read, maybe)

import Control.Applicative (Applicative, Alternative, (<$))
import Control.Monad (MonadPlus, mzero, mfilter)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.State (MonadState, StateT(..))
import qualified Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import qualified Data.Text.Read as Text
import Network.HTTP.Types (Method, StdMethod(..), renderStdMethod)
import Network.Wai (Request, pathInfo, requestMethod)
import Text.Read (readMaybe)

newtype RouteM a = RouteM { runRoute :: ReaderT Method (StateT [Text] Maybe) a }
  deriving (Functor, Monad, MonadPlus, Applicative, Alternative, MonadReader Method, MonadState [Text])

method :: RouteM Method
method = ask

on :: StdMethod -> RouteM Method
on s = mfilter (renderStdMethod s ==) method

text :: RouteM Text
text = RouteM $ lift $ StateT f where
  f (p:l) = Just (p, l)
  f [] = Nothing

maybe :: MonadPlus m => Maybe a -> m a
maybe = Data.Maybe.maybe mzero return

fixed :: Text -> RouteM Text
fixed p = mfilter (p ==) text

instance IsString (RouteM a) where
  fromString s = undefined <$ fixed (fromString s)

path :: RouteM [Text]
path = RouteM $ lift $ StateT $ \p -> Just (p, [])

read :: Read a => RouteM a
read = maybe . readMaybe . unpack =<< text

reader :: Text.Reader a -> RouteM a
reader r = either (const mzero) (return . fst) . r =<< text

class Routable a where
  route :: RouteM a

routeRequest :: RouteM a -> Request -> Maybe a
routeRequest r q = case runStateT (runReaderT (runRoute r) (requestMethod q)) (pathInfo q) of
  Just (a, []) -> Just a
  _ -> Nothing
