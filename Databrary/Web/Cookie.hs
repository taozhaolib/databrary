module Databrary.Web.Cookie
  ( getSignedCookie
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Web.Cookie as Cook

import Databrary.Crypto
import Databrary.Web.Wai

getCookies :: HasRequest m => m Cook.Cookies
getCookies = maybe [] Cook.parseCookies <$> getRequestHeader "cookie"

getSignedCookie :: HasRequest m => BS.ByteString -> m (Maybe T.Text)
getSignedCookie c = unSignText . lookup c <$> getCookies
