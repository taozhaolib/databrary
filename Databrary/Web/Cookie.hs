{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Cookie
  ( getSignedCookie
  ) where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Web.Cookie as Cook

import Databrary.Crypto
import Databrary.Resource
import Databrary.Action

getCookies :: RequestM m => m Cook.Cookies
getCookies = maybe [] Cook.parseCookies `liftM` getRequestHeader "cookie"

getSignedCookie :: (ResourceM m, RequestM m) => BS.ByteString -> m (Maybe BS.ByteString)
getSignedCookie c = maybe (return Nothing) unSign . lookup c =<< getCookies
