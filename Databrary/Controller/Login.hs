{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( postLogin
  , viewLogin
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Digestive as Form

import Control.Has (view)
import Databrary.Action
import Databrary.Web.Form
import Databrary.Web.Cookie
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Token
import Databrary.View.Form
import Databrary.Controller.Form

loginAccount :: SiteAuth -> Bool -> AppAction
loginAccount auth su = do
  sess <- createSession auth su
  let Token tok ex = view sess
  cook <- setSignedCookie "session" tok ex
  okResponse [cook] (mempty :: Blaze.Builder)

data LoginForm = LoginForm
  { loginEmail :: T.Text
  , loginPassword :: T.Text
  , loginSuperuser :: Bool
  }

loginForm :: Monad m => Form.Form T.Text m LoginForm
loginForm = LoginForm
  <$> "email"     Form..: emailTextForm Nothing
  <*> "password"  Form..: Form.text Nothing
  <*> "superuser" Form..: Form.bool (Just False)

displayLogin :: ActionM c m => Bool -> Form.View T.Text -> m Response
displayLogin api = displayForm api $
  renderForm (postLogin api)
    [ ("email", inputText)
    , ("password", inputPassword)
    ]

viewLogin :: Bool -> AppRAction
viewLogin api = action GET (apiRoute api ["login"]) $ do
  form <- Form.getForm "login" loginForm
  displayLogin api form

postLogin :: Bool -> AppRAction
postLogin api = action POST (apiRoute api ["login"]) $ do
  (login, form) <- runForm "login" disp loginForm
  auth <- lookupSiteAuthByEmail (loginEmail login)
  let p = fmap view auth
      a = partyAccount =<< p
      su = loginSuperuser login && Fold.any ((PermissionADMIN ==) . view) auth
  attempts <- maybe (return 0) recentAccountLogins p
  let pass = maybe False (flip BCrypt.validatePassword (TE.encodeUtf8 (loginPassword login))) (accountPasswd =<< a)
      block = attempts > 4
  auditAccountLogin pass (fromMaybe nobodyParty p) (loginEmail login)
  when block $ result =<< disp (formAddError ["email"] "Too many login attempts. Try again later." form)
  unless pass $ result =<< disp (formAddError ["password"] "Incorrect login." form)
  loginAccount (fromJust auth) su
  where disp = displayLogin api
