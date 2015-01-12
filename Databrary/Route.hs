module Databrary.Route
  ( RouteM
  , fixed
  , text
  , path
  , runRoute
  ) where

import Control.Monad.State
import Data.Text (Text)
import Network.Wai (Request, pathInfo)

type RouteM = StateT [Text] Maybe

text :: RouteM Text
text = StateT f where
  f (p:l) = Just (p, l)
  f [] = Nothing

fixed :: Text -> RouteM ()
fixed p' = guard . (p' ==) =<< text

path :: RouteM [Text]
path = StateT $ \p -> Just (p, [])

runRoute :: RouteM a -> Request -> Maybe a
runRoute r q = case runStateT r (pathInfo q) of
  Just (a, []) -> Just a
  _ -> Nothing
