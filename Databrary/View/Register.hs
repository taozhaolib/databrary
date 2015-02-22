{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Register
  ( htmlRegister
  ) where

import Databrary.Action.Auth
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Register

htmlRegister :: AuthRequest -> FormHtml
htmlRegister req = htmlForm "Register" (postRegister False) req $ do
  field "name" $ inputText (Nothing :: Maybe String)
  field "email" $ inputText (Nothing :: Maybe String)
  field "affiliation" $ inputText (Nothing :: Maybe String)
  field "agreement" $ inputCheckbox False
