{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Digestive as Form

import Control.Has (see)
import Databrary.Action
import Databrary.Web.Form
import Databrary.Web.Cookie
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Authorize
import Databrary.Model.Token

loginAccount :: PartyAuth -> Bool -> AppBAction
loginAccount auth su = do
  sess <- createSession auth su
  let Token tok ex = see sess
  setSignedCookie "session" tok ex
  okResult

data LoginForm = LoginForm
  { _loginEmail :: T.Text
  , _loginPassword :: T.Text
  , _loginSuperuser :: Bool
  }

loginForm :: Monad m => Form.Form T.Text m LoginForm
loginForm = LoginForm
  <$> "email"     Form..: emailTextForm
  <*> "password"  Form..: Form.text Nothing
  <*> "superuser" Form..: Form.bool (Just False)

postLogin :: AppRAction
postLogin = bAction POST ["login"] $ do
  (LoginForm email password superuser, form) <- runForm "login" loginForm
  auth <- lookupPartyAuthByEmail email
  let p = fmap see auth
      a = partyAccount =<< p
      su = superuser && Fold.any ((PermissionADMIN ==) . accessPermission) auth
  attempts <- maybe (return 0) recentAccountLogins p
  let pass = maybe False (flip BCrypt.validatePassword (TE.encodeUtf8 password)) (accountPasswd =<< a)
      block = attempts > 4
  auditAccountLogin pass (fromMaybe nobodyParty p) email
  when block $ resultFormError $ formAddError [] "Too many login attempts. Try again later." form
  unless pass $ resultFormError $ formAddError [] "Incorrect login." form
  loginAccount (fromJust auth) su
