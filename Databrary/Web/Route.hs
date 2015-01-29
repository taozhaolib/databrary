{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Databrary.Web.Route
  ( RouteM

  , method
  , on, StdMethod(..)

  , text
  , fixed
  , switch
  , path
  , read
  , reader
  , Routable(..)
  , query

  , routeRequest
  ) where

import Prelude hiding (read, maybe)

import Control.Applicative (Applicative, Alternative, (<$))
import Control.Monad (MonadPlus, mzero, mfilter)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks)
import Control.Monad.State (MonadState, StateT(..))
import qualified Data.ByteString as BS
import Data.Maybe (maybe, fromMaybe)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Read as Text
import Network.HTTP.Types (Method, StdMethod(..), renderStdMethod)
import qualified Network.Wai as Wai
import Text.Read (readMaybe)

newtype RouteM a = RouteM { runRoute :: ReaderT Wai.Request (StateT [T.Text] Maybe) a }
  deriving (Functor, Monad, MonadPlus, Applicative, Alternative, MonadReader Wai.Request, MonadState [T.Text])

method :: RouteM Method
method = asks Wai.requestMethod

on :: StdMethod -> RouteM Method
on s = mfilter (renderStdMethod s ==) method

text :: RouteM T.Text
text = RouteM $ lift $ StateT f where
  f (p:l) = Just (p, l)
  f [] = Nothing

fixed :: T.Text -> RouteM T.Text
fixed p = mfilter (p ==) text

instance IsString (RouteM a) where
  fromString s = undefined <$ fixed (fromString s)

switch :: Eq a => [(a, RouteM b)] -> a -> RouteM b
switch l x = fromMaybe mzero $ lookup x l

path :: RouteM [T.Text]
path = RouteM $ lift $ StateT $ \p -> Just (p, [])

read :: Read a => RouteM a
read = maybe mzero return . readMaybe . T.unpack =<< text

reader :: Text.Reader a -> RouteM a
reader r = either (const mzero) (return . fst) . r =<< text

class Routable a where
  route :: RouteM a
  toRoute :: a -> [T.Text]

query :: BS.ByteString -> RouteM BS.ByteString
query k = maybe mzero (return . fromMaybe "") . lookup k =<< asks Wai.queryString

routeRequest :: RouteM a -> Wai.Request -> Maybe a
routeRequest r q = case runStateT (runReaderT (runRoute r) q) (Wai.pathInfo q) of
  Just (a, []) -> Just a
  _ -> Nothing
