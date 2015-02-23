{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( loginAccount
  , viewLogin
  , postLogin
  , postLogout
  ) where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)

import Control.Applicative.Ops
import Control.Has (view)
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Web.Cookie
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Token
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.View.Login

import {-# SOURCE #-} Databrary.Controller.Root
import {-# SOURCE #-} Databrary.Controller.Party

loginAccount :: Bool -> SiteAuth -> Bool -> AppAction
loginAccount api auth su = do
  sess <- createSession auth su
  let Token (Id tok) ex = view sess
  cook <- setSignedCookie "session" tok ex
  if api
    then okResponse [cook] $ identityJSON (Identified sess)
    else redirectRouteResponse [cook] $ viewParty False Nothing

viewLogin :: AppRAction
viewLogin = action GET ["login"] $ withAuth $
  maybeIdentity
    (blankForm htmlLogin)
    (\_ -> redirectRouteResponse [] $ viewParty False Nothing)

postLogin :: Bool -> AppRAction
postLogin api = action POST (apiRoute api ["login"]) $ do
  (Just auth, su) <- withoutAuth $ runForm (api ?!> htmlLogin) $ do
    email <- "email" .:> emailTextForm
    password <- "password" .:> deform
    superuser <- "superuser" .:> deform
    auth <- lift $ lookupSiteAuthByEmail email
    let p = view <$> auth
        a = partyAccount =<< p
        su = superuser && Fold.any ((PermissionADMIN ==) . view) auth
    attempts <- lift $ maybe (return 0) recentAccountLogins p
    let pass = maybe False (flip BCrypt.validatePassword password) (accountPasswd =<< a)
        block = attempts > 4
    lift $ auditAccountLogin pass (fromMaybe nobodyParty p) email
    when block $ "email" .:> deformError "Too many login attempts. Try again later."
    unless pass $ "password" .:> deformError "Incorrect login."
    return (auth, su)
  loginAccount api auth su

postLogout :: Bool -> AppRAction
postLogout api = action POST (apiRoute api ["logout"]) $ withAuth $ do
  maybeIdentity (return False) removeSession
  if api
    then okResponse [cook] $ identityJSON UnIdentified
    else redirectRouteResponse [cook] $ viewRoot False
  where cook = clearCookie "session"
