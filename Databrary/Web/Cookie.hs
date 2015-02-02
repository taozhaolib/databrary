{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Cookie
  ( getSignedCookie
  , setSignedCookie
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Network.HTTP.Types (Header, hCookie)
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cook

import Control.Has (peeks)
import Databrary.Crypto
import Databrary.Resource
import Databrary.Time
import Databrary.Action.Request

getCookies :: RequestM c m => m Cook.Cookies
getCookies = maybe [] Cook.parseCookies <$> getRequestHeader hCookie

getSignedCookie :: (ResourceM c m, RequestM c m) => BS.ByteString -> m (Maybe BS.ByteString)
getSignedCookie c = maybe (return Nothing) unSign . lookup c =<< getCookies

setSignedCookie :: (ResourceM c m, RequestM c m) => BS.ByteString -> BS.ByteString -> Timestamp -> m Header
setSignedCookie c val ex = do
  val' <- sign val
  sec <- peeks Wai.isSecure
  return ("set-cookie", Blaze.toByteString $ Cook.renderSetCookie $ Cook.def
    { Cook.setCookieName = c
    , Cook.setCookieValue = val'
    , Cook.setCookiePath = Just "/"
    , Cook.setCookieExpires = Just ex
    , Cook.setCookieSecure = sec
    })
