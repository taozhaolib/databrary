{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Register
  ( viewRegister
  , postRegister
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>), mempty)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Network.Mail.Mime as Mail

import Control.Applicative.Ops
import Control.Has (view, peeks)
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Token
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.Controller.Token
import Databrary.View.Register

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
  tok <- loginTokenId =<< createLoginToken auth True
  url <- peeks $ actionURL $ viewLoginToken False tok
  liftIO $ Mail.renderSendMail $ Mail.simpleMail'
    (Mail.Address (Just (partyName (view auth))) (accountEmail (view auth)))
    (Mail.Address (Just "Databrary") "help@databrary.org")
    "Databrary account created"
    ("Thank you for registering with Databrary. Please use this link to complete your\n\
      \registration:\n\n"
      <> TL.fromStrict (TE.decodeLatin1 url) <> "\n\n\
      \By clicking the above link, you also indicate that you have read and understand\n\
      \the Databrary Access agreement, which you can download here:\n\n\
      \http://databrary.org/policies/agreement.pdf\n\n\
      \Once you've validated your e-mail, you will be able to request authorization in\n\
      \order to be granted full access to Databrary.")
  okResponse [] $ "Your confirmation email has been sent to '" <> accountEmail reg <> "'."
