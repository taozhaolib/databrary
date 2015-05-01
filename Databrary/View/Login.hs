{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Login
  ( htmlLogin
  , htmlUserForm
  ) where

import Databrary.Model.Party.Types
import Databrary.HTTP.Form.View
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Login

htmlLogin :: AuthRequest -> FormHtml
htmlLogin req = htmlForm "Login" postLogin HTML req $ do
  field "email" $ inputText (Nothing :: Maybe String)
  field "password" inputPassword
  field "superuser" $ inputCheckbox False

htmlUserForm :: Account -> AuthRequest -> FormHtml
htmlUserForm a req = htmlForm "Edit account" postUser HTML req $ do
  field "auth" $ inputPassword
  field "email" $ inputText $ Just $ accountEmail a
  "password" .:> do
    field "once" inputPassword
    field "again" inputPassword
