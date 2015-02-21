{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Login
  ( renderLogin
  ) where

import Databrary.Action
import Databrary.View.Form

renderLogin :: RouteAction q -> FormHtml
renderLogin act = renderForm act $ do
  field "email" inputText
  field "password" inputPassword
  field "superuser" inputCheckbox
