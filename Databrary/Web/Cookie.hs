{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Cookie
  ( getSignedCookie
  , setSignedCookie
  , clearCookie
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import Network.HTTP.Types (Header, hCookie)
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cook

import Control.Has (peeks)
import Databrary.Crypto
import Databrary.Resource
import Databrary.Model.Time.Types
import Databrary.Web.Request

getCookies :: Request -> Cook.Cookies
getCookies = maybe [] Cook.parseCookies . lookupRequestHeader hCookie

getSignedCookie :: (MonadHasResource c m, MonadHasRequest c m) => BS.ByteString -> m (Maybe BS.ByteString)
getSignedCookie c = maybe (return Nothing) unSign . lookup c =<< peeks getCookies

setSignedCookie :: (MonadHasResource c m, MonadHasRequest c m) => BS.ByteString -> BS.ByteString -> Timestamp -> m Header
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

clearCookie :: BS.ByteString -> Header
clearCookie c = ("set-cookie", Blaze.toByteString $ Cook.renderSetCookie $ Cook.def
  { Cook.setCookieName = c
  , Cook.setCookiePath = Just "/"
  })
