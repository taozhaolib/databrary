{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Angular
  ( jsURL
  , angular
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (encodePath, hUserAgent)
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Ops
import Databrary.Has (peek, view)
import Databrary.Web.Uglify
import Databrary.Action
import Databrary.HTTP.Request
import Databrary.View.Angular

checkQueryValue :: BS.ByteString -> Bool
checkQueryValue "0" = False
checkQueryValue "false" = False
checkQueryValue "off" = False
checkQueryValue "" = False
checkQueryValue _ = True

checkQueryParameter :: Maybe BS.ByteString -> Bool
checkQueryParameter = Fold.all checkQueryValue

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, BSB.Builder)
jsURL js req =
  second (encodePath (Wai.pathInfo req) . maybe id (\v -> (("js", Just (if v then "1" else "0")) :)) js)
  $ unjs $ Wai.queryString req where
  unjs [] = (Nothing, [])
  unjs (("js",v):q) = (Just (checkQueryParameter v), snd $ unjs q)
  unjs (x:q) = second (x:) $ unjs q

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnable :: Wai.Request -> Bool
angularEnable = not . Fold.any (Regex.matchTest browserBlacklist) . lookupRequestHeader hUserAgent

angular :: (MonadIO m, MonadAuthAction q m) => m ()
angular = do
  auth <- peek
  let req = view auth
      (js, nojs) = jsURL (Just False) req
      js' = fromMaybe (angularEnable req) js
  when js' $ do
    debug <-
#ifdef DEVEL
      (any checkQueryParameter $ lookupQueryParameters "debug" req)
#else
      False
#endif
      ?$> liftIO allWebJS
    result =<< okResponse [] (htmlAngular debug nojs auth)
