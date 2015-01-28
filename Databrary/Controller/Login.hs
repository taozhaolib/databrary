{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  , viewLogin
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (ok200)
import qualified Text.Digestive as Form

import Control.Has (see)
import Databrary.Action
import Databrary.Web.Form
import Databrary.Web.Cookie
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Authorize
import Databrary.Model.Token
import Databrary.View.Form
import Databrary.Controller.Form

loginAccount :: PartyAuth -> Bool -> AppBAction
loginAccount auth su = do
  sess <- createSession auth su
  let Token tok ex = see sess
  setSignedCookie "session" tok ex
  return ok200

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

displayLogin :: Monad m => Bool -> Form.View T.Text -> BResult q m
displayLogin api = displayForm api $
  renderForm (postLogin api)
    [ ("email", inputText)
    , ("password", inputPassword)
    ]

viewLogin :: Bool -> AppRAction
viewLogin api = bAction GET (apiRoute api ["login"]) $ do
  form <- Form.getForm "login" loginForm
  displayLogin api form

postLogin :: Bool -> AppRAction
postLogin api = bAction POST (apiRoute api ["login"]) $ do
  (LoginForm email password superuser, form) <- runForm "login" disp loginForm
  auth <- lookupPartyAuthByEmail email
  let p = fmap see auth
      a = partyAccount =<< p
      su = superuser && Fold.any ((PermissionADMIN ==) . accessPermission) auth
  attempts <- maybe (return 0) recentAccountLogins p
  let pass = maybe False (flip BCrypt.validatePassword (TE.encodeUtf8 password)) (accountPasswd =<< a)
      block = attempts > 4
  auditAccountLogin pass (fromMaybe nobodyParty p) email
  when block $ resultWith $ disp $ formAddError ["email"] "Too many login attempts. Try again later." form
  unless pass $ resultWith $ disp $ formAddError ["password"] "Incorrect login." form
  loginAccount (fromJust auth) su
  where disp = displayLogin api
