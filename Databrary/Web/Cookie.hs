{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Databrary.Web.Cookie
  ( getSignedCookie
  ) where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Web.Cookie as Cook

import Databrary.Crypto
import Databrary.Resource
import Databrary.Action.Types

getCookies :: RequestM c m => m Cook.Cookies
getCookies = maybe [] Cook.parseCookies `liftM` getRequestHeader "cookie"

getSignedCookie :: (ResourceM c m, RequestM c m) => BS.ByteString -> m (Maybe BS.ByteString)
getSignedCookie c = maybe (return Nothing) unSign . lookup c =<< getCookies
