{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Angular
  ( jsURL
  , angular
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Builder as BSB
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (encodePath, hUserAgent)
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Has (peeks)
import Databrary.Action
import Databrary.HTTP.Request
import Databrary.View.Angular

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, BSB.Builder)
jsURL js req =
  second (encodePath (Wai.pathInfo req) . maybe id (\v -> (("js", Just (if v then "1" else "0")) :)) js)
  $ unjs $ Wai.queryString req where
  unjs [] = (Nothing, [])
  unjs (("js",v):q) = (Just (bool v), snd $ unjs q)
  unjs (x:q) = second (x:) $ unjs q
  bool (Just "0") = False
  bool (Just "false") = False
  bool (Just "off") = False
  bool (Just "") = False
  bool _ = True

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnabled :: Wai.Request -> Bool
angularEnabled req = fromMaybe def $ fst $ jsURL Nothing req where
  def = not $ Fold.any (Regex.matchTest browserBlacklist) $ lookupRequestHeader hUserAgent req

viewAngular :: MonadAuthAction q m => m Response
viewAngular =
  okResponse [] =<< peeks htmlAngular 

angular :: (MonadIO m, MonadAuthAction q m) => m ()
angular = do
  js <- peeks angularEnabled
  when js $ result =<< viewAngular
