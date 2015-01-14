module Databrary.Identity
  ( module Databrary.Types.Identity
  , getIdentity
  ) where

import Databrary.Action
import Databrary.Web.Cookies

getIdentity :: ActionM Identity
getIdentity = do
  session <- getSignedCookie "session"
  getSession session
