{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings, UndecidableInstances #-}
module Databrary.HTTP.Route
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
  , readQuery

  , routeRequest
  ) where

import Prelude hiding (read, maybe)

import Control.Applicative (Applicative(..), Alternative(..), (<$), (<$>), optional)
import Control.Arrow (first)
import Control.Monad ((<=<), MonadPlus(..), mzero, mfilter, ap, join, guard)
import Control.Monad.Reader.Class (MonadReader(..), asks)
import Control.Monad.State.Class (MonadState(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64, Int32, Int16)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Text
import Network.HTTP.Types (Method)
import qualified Network.Wai as Wai
import Text.Read (readMaybe)

import Databrary.Model.Offset
import Databrary.Model.Segment

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

instance Routable () where
  route = return ()
  toRoute () = []

instance Routable a => Routable (Maybe a) where
  route = optional route
  toRoute Nothing = []
  toRoute (Just a) = toRoute a

instance (Routable a, Routable b) => Routable (a, b) where
  route = (,) <$> route <*> route
  toRoute (a, b) = toRoute a <> toRoute b

instance (Routable a, Routable b, Routable c) => Routable (a, b, c) where
  route = (,,) <$> route <*> route <*> route
  toRoute (a, b, c) = toRoute a <> toRoute b <> toRoute c

instance (Routable a, Routable b, Routable c, Routable d) => Routable (a, b, c, d) where
  route = (,,,) <$> route <*> route <*> route <*> route
  toRoute (a, b, c, d) = toRoute a <> toRoute b <> toRoute c <> toRoute d

instance Routable T.Text where
  route = text
  toRoute = return

instance Routable [T.Text] where
  route = path
  toRoute = id

instance Routable BS.ByteString where
  route = TE.encodeUtf8 <$> text
  toRoute = return . TE.decodeUtf8

instance Routable Integer where
  route = readText (Text.signed Text.decimal)
  toRoute = return . T.pack . show

instance Routable Int where
  route = readText (Text.signed Text.decimal)
  toRoute = return . T.pack . show

instance Routable Int64 where
  route = readText (Text.signed Text.decimal)
  toRoute = return . T.pack . show

instance Routable Int32 where
  route = readText (Text.signed Text.decimal)
  toRoute = return . T.pack . show

instance Routable Int16 where
  route = readText (Text.signed Text.decimal)
  toRoute = return . T.pack . show

instance Routable Offset where
  route = read
  toRoute = return . T.pack . show . offsetMillis

instance Routable Segment where
  route = read
  toRoute s = return $ T.pack $ showSegmentWith (shows . offsetMillis) s ""

query :: BS.ByteString -> RouteM BS.ByteString
query k = maybe . (fmap $ fromMaybe "") . lookup k =<< asks Wai.queryString

readQuery :: Read a => BS.ByteString -> RouteM a
readQuery k = maybe . readMaybe . BSC.unpack =<< query k

routeRequest :: RouteM a -> Wai.Request -> Maybe a
routeRequest (RouteM r) q = fst <$> r q (filter (not . T.null) $ Wai.pathInfo q)
