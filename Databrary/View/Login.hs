{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Login
  ( htmlLogin
  ) where

import Databrary.Action.Auth
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Login

htmlLogin :: AuthRequest -> FormHtml
htmlLogin req = htmlForm "Login" (postLogin False) req $ do
  field "email" $ inputText (Nothing :: Maybe String)
  field "password" inputPassword
  field "superuser" $ inputCheckbox False
