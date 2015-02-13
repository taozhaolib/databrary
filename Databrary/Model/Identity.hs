{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Identity
  ( module Databrary.Model.Identity.Types
  , determineIdentity
  ) where

import Databrary.Model.Token
import Databrary.Action.Request
import Databrary.Resource
import Databrary.DB
import Databrary.Web.Cookie
import Databrary.Model.Identity.Types

determineIdentity :: (MonadHasResource c m, MonadHasRequest c m, DBM m) => m Identity
determineIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  return $ maybe UnIdentified Identified s
