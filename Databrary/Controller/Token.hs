{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Token
  ( viewLoginToken
  , postPasswordToken
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (isJust)

import Control.Applicative.Ops
import Control.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Crypto
import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Model.Party
import Databrary.Action
import Databrary.Action.Route
import Databrary.Action.Auth
import Databrary.Controller.Form
import Databrary.Controller.Login
import Databrary.View.Token

viewLoginToken :: Bool -> Id LoginToken -> AppRAction
viewLoginToken api ti = action GET (apiRoute api $ toRoute ti) $ do
  tok <- maybeAction =<< lookupLoginToken ti
  if loginPasswordToken tok
    then if api
      then okResponse [] $ JSON.record ti
        [ "reset" JSON..= isJust (accountPasswd (view tok))
        ]
      else withoutAuth $ blankForm $ htmlPasswordToken ti
    else do
      removeLoginToken tok
      loginAccount api (view tok) False

postPasswordToken :: Bool -> Id LoginToken -> AppRAction
postPasswordToken api ti = action POST (apiRoute api $ toRoute ti) $ do
  tok <- maybeAction =<< lookupLoginToken ti
  guardAction (loginPasswordToken tok) notFoundResponse
  p <- withoutAuth $ runForm (api ?!> htmlPasswordToken ti) $
    passwordForm
  pw <- liftIO $ BCrypt.hashPasswordUsingPolicy passwordPolicy p
  withoutAuth $ changeAccount (view tok) { accountPasswd = pw } -- or should this be withAuth?
  loginAccount api (view tok) False
