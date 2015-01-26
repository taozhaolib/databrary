{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless)
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Digestive as Form

import Control.Has (see)
import Databrary.Action
import Databrary.Web.Form
import Databrary.Model.Party

data LoginForm = LoginForm
  { loginEmail :: T.Text
  , loginPassword :: T.Text
  }

loginAccount :: PartyAuth -> AppBAction
loginAccount a = do
  return undefined

loginForm :: Monad m => Form.Form T.Text m LoginForm
loginForm = LoginForm
  <$> "email"    Form..: emailTextForm
  <*> "password" Form..: Form.text Nothing

postLogin :: AppBAction
postLogin = do
  (LoginForm email password, form) <- runForm "login" loginForm
  auth <- lookupPartyAuthByEmail email
  let p = fmap see auth
      a = partyAccount =<< p
  attempts <- maybe (return 0) recentAccountLogins p
  let pass = maybe False (flip BCrypt.validatePassword (TE.encodeUtf8 password)) (accountPasswd =<< a)
      block = attempts > 4
  auditAccountLogin pass (fromMaybe nobodyParty p) email
  when block $ resultFormError $ formAddError [] "Too many login attempts. Try again later." form
  unless pass $ resultFormError $ formAddError [] "Incorrect login." form
  loginAccount $ fromJust auth
