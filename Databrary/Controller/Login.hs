{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Text.Digestive as Form

import Databrary.Action
import Databrary.Web.Form

data LoginForm = LoginForm
  { loginEmail :: T.Text
  , loginPassword :: T.Text
  }

loginForm :: Monad m => Form.Form T.Text m LoginForm
loginForm = LoginForm
  <$> "email" Form..: emailTextForm
  <*> "password" Form..: Form.text Nothing

postLogin :: AppBAction
postLogin = do
  LoginForm email pass <- runForm "login" loginForm
  return $ undefined
