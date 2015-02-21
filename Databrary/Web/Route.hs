{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings, UndecidableInstances #-}
module Databrary.Web.Route
  ( RouteM

  , maybe
  , method
  , final
  , text
  , fixed
  , switch
  , path
  , read
  , readText
  , Routable(..)
  , query

  , routeRequest
  ) where

import Prelude hiding (read, maybe)

import Control.Applicative (Applicative(..), Alternative(..), (<$), (<$>))
import Control.Arrow (first)
import Control.Monad ((<=<), MonadPlus(..), mzero, mfilter, ap, join, guard)
import Control.Monad.Reader.Class (MonadReader(..), asks)
import Control.Monad.State.Class (MonadState(..))
import qualified Data.ByteString as BS
import Data.Int (Int32, Int16)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Read as Text
import Network.HTTP.Types (Method)
import qualified Network.Wai as Wai
import Text.Read (readMaybe)

newtype RouteM a = RouteM { runRouteM :: Wai.Request -> [T.Text] -> Maybe (a, [T.Text]) }

lpt :: e -> a -> (a, e)
lpt e a = (a, e)

instance Functor RouteM where
  fmap f (RouteM a) = RouteM $ \q -> fmap (first f) . a q

instance Applicative RouteM where
  pure = return
  (<*>) = ap

instance Monad RouteM where
  return a = RouteM $ \_ p -> Just (a, p)
  RouteM x >>= f = RouteM $ \q -> 
    let rf (vx, px) = runRouteM (f vx) q px in rf <=< x q
  fail _ = mzero

instance MonadPlus RouteM where
  mzero = RouteM $ \_ _ -> Nothing
  RouteM a `mplus` RouteM b = RouteM $ \q t ->
    a q t `mplus` b q t

instance Alternative RouteM where
  empty = mzero
  (<|>) = mplus

instance MonadReader Wai.Request RouteM where
  ask = RouteM $ \q -> Just . (,) q
  reader f = RouteM $ \q -> Just . (,) (f q)
  local f (RouteM a) = RouteM $ a . f

instance MonadState [T.Text] RouteM where
  get = RouteM $ \_ -> Just . join (,)
  put t = RouteM $ \_ _ -> Just ((), t)
  state f = RouteM $ \_ -> Just . f

maybe :: Maybe a -> RouteM a
maybe r = RouteM $ \_ t -> fmap (lpt t) r

method :: RouteM Method
method = asks Wai.requestMethod

text :: RouteM T.Text
text = RouteM $ \_ -> f where
  f (p:l) = Just (p, l)
  f [] = Nothing

fixed :: T.Text -> RouteM T.Text
fixed p = mfilter (p ==) text

instance IsString (RouteM a) where
  fromString s = undefined <$ fixed (fromString s)

switch :: Eq a => [(a, RouteM b)] -> a -> RouteM b
switch l x = fromMaybe mzero $ lookup x l

path :: RouteM [T.Text]
path = RouteM $ \_ p -> Just (p, [])

final :: RouteM ()
final = path >>= guard . null

read :: Read a => RouteM a
read = maybe . readMaybe . T.unpack =<< text

readText :: Text.Reader a -> RouteM a
readText r = either (const mzero) (return . fst) . r =<< text

class Routable a where
  route :: RouteM a
  toRoute :: a -> [T.Text]

instance Routable Integer where
  route = readText (Text.signed Text.decimal)
  toRoute  = return . T.pack . show

instance Routable Int where
  route = readText (Text.signed Text.decimal)
  toRoute  = return . T.pack . show

instance Routable Int32 where
  route = readText (Text.signed Text.decimal)
  toRoute  = return . T.pack . show

instance Routable Int16 where
  route = readText (Text.signed Text.decimal)
  toRoute  = return . T.pack . show

query :: BS.ByteString -> RouteM BS.ByteString
query k = maybe . (fmap $ fromMaybe "") . lookup k =<< asks Wai.queryString

routeRequest :: RouteM a -> Wai.Request -> Maybe a
routeRequest (RouteM r) q = fst <$> r q (Wai.pathInfo q)
