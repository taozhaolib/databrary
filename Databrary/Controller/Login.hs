{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  ) where

import Databrary.Action
import Databrary.Web.Form

postLogin :: AppBAction
postLogin = do
  postForm "login" undefined
  return $ undefined
