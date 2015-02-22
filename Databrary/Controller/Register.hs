{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Register
  ( viewRegister
  , postRegister
  ) where

import Data.Monoid ((<>), mempty)

import Control.Applicative.Ops
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Token
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.View.Register

import {-# SOURCE #-} Databrary.Controller.Party

viewRegister :: AppRAction
viewRegister = action GET ["register"] $ withAuth $
  maybeIdentity
    (blankForm htmlRegister)
    (\_ -> redirectRouteResponse [] $ viewParty False Nothing)

postRegister :: Bool -> AppRAction
postRegister api = action POST (apiRoute api ["register"]) $ withoutAuth $ do
  reg <- runForm (api ?!> htmlRegister) $ do
    name <- "name" .:> deform
    email <- "email" .:> emailTextForm
    affiliation <- "affiliation" .:> deform
    _ <- "agreement" .:> (deformCheck "You must consent to the user agreement." id =<< deform)
    let p = blankParty
          { partyName = name
          , partyAffiliation = affiliation
          , partyAccount = Just a
          }
        a = Account
          { accountParty = p
          , accountEmail = email
          , accountPasswd = Nothing
          }
    return a
  auth <- maybe (flip SiteAuth mempty <$> addAccount reg) return =<< lookupSiteAuthByEmail (accountEmail reg)
  tok <- createLoginToken auth True
  okResponse [] $ "Your confirmation email has been sent to '" <> accountEmail reg <> "'."
