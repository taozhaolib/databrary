{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Token
  ( viewLoginToken
  , postPasswordToken
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (withReaderT)
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
import Databrary.Action.Auth
import Databrary.Controller.Form
import Databrary.Controller.Login
import Databrary.Controller.Angular
import Databrary.View.Token

viewLoginToken :: API -> Id LoginToken -> AppRAction
viewLoginToken api ti = action GET (api, ti) $ withoutAuth $ do
  when (api == HTML) angular
  tok <- maybeAction =<< lookupLoginToken ti
  if loginPasswordToken tok
    then case api of
      JSON -> okResponse [] $ JSON.record ti
        [ "reset" JSON..= isJust (accountPasswd (view tok))
        ]
      HTML -> blankForm $ htmlPasswordToken ti
    else do
      removeLoginToken tok
      withReaderT authApp $ loginAccount api (view tok) False

postPasswordToken :: API -> Id LoginToken -> AppRAction
postPasswordToken api ti = action POST (api, ti) $ withoutAuth $ do
  tok <- maybeAction =<< lookupLoginToken ti
  guardAction (loginPasswordToken tok) notFoundResponse
  p <- runForm (api == HTML ?> htmlPasswordToken ti) $
    passwordForm
  pw <- liftIO $ BCrypt.hashPasswordUsingPolicy passwordPolicy p
  changeAccount (view tok) { accountPasswd = pw } -- or should this be withAuth?
  removeLoginToken tok
  withReaderT authApp $ loginAccount api (view tok) False
