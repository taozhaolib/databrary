{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Template
  ( htmlHeader
  , htmlFooter
  , htmlTemplate
  ) where

import Control.Monad (when)
import qualified Data.Foldable as Fold
import Data.Maybe (isJust)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.HTTP.Types (methodGet)
import qualified Network.Wai as Wai

import Databrary.Has (view)
import Databrary.Model.Identity
import Databrary.Action.Auth
import Databrary.Action
import Databrary.View.Html

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Login
import {-# SOURCE #-} Databrary.Controller.Party

htmlHeader :: Wai.Request -> H.Html
htmlHeader req = do
  when (Wai.requestMethod req == methodGet && isJust hasjs) $
    H.link
      H.! HA.rel "canonical"
      H.! HA.href (builderValue nojs)
  H.link
    H.! HA.rel "shortcut icon"
    H.! HA.href "/public/icons/favicon.png"
  H.link
    H.! HA.rel "start"
    H.! HA.href "/"
  where
  (hasjs, nojs) = jsURL Nothing req

htmlFooter :: H.Html
htmlFooter = mempty

htmlTemplate :: AuthRequest -> Maybe T.Text -> H.Html -> H.Html
htmlTemplate req title body = H.docTypeHtml $ do
  H.head $ do
    htmlHeader (view req)
    H.title $ do
      Fold.mapM_ (\t -> H.toHtml t >> " || ") title
      "Databrary"
  H.body $ do
    H.section
      H.! HA.id "toolbar"
      H.! HA.class_ "toolbar"
      $ do
        H.a
          H.! HA.href "/"
          $ "Databrary"
        foldIdentity
          (H.a H.! actionLink viewLogin () $ "login")
          (\_ -> do
            H.a H.! actionLink viewParty (HTML, TargetProfile) $ "profile"
            actionForm postLogout HTML $
              H.button
                H.! HA.type_ "submit"
                $ "logout")
          $ authIdentity req
    Fold.mapM_ (H.h1 . H.toHtml) title
    body
    htmlFooter
