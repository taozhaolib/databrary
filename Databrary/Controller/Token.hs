{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Token
  ( viewLoginToken
  , postPasswordToken
  ) where

import Control.Monad (when)
import Control.Monad.Reader (withReaderT)
import Data.Maybe (isJust)

import Databrary.Ops
import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Model.Party
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Login
import Databrary.Controller.Angular
import Databrary.View.Token

viewLoginToken :: AppRoute (API, Id LoginToken)
viewLoginToken = action GET (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  when (api == HTML) angular
  tok <- maybeAction =<< lookupLoginToken ti
  if loginPasswordToken tok
    then case api of
      JSON -> okResponse [] $ JSON.record ti
        [ "reset" JSON..= isJust (accountPasswd (view tok))
        ]
      HTML -> blankForm $ htmlPasswordToken ti
    else do
      _ <- removeLoginToken tok
      withReaderT authApp $ loginAccount api (view tok) False

postPasswordToken :: AppRoute (API, Id LoginToken)
postPasswordToken = action POST (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  tok <- maybeAction =<< lookupLoginToken ti
  guardAction (loginPasswordToken tok) notFoundResponse
  let acct = view tok
  pw <- runForm (api == HTML ?> htmlPasswordToken ti) $
    passwordForm acct
  changeAccount acct{ accountPasswd = Just pw } -- or should this be withAuth?
  _ <- removeLoginToken tok
  withReaderT authApp $ loginAccount api (view tok) False
