{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)
import qualified Data.Text as T
import Network.URI (URI)
import qualified Text.Digestive as Form

import Databrary.Action
import Databrary.Web.Form
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

citeForm :: Monad m => Form.Form T.Text m URI
citeForm = "url" Form..: urlForm Nothing

getCitation :: AppRAction
getCitation = action GET ["api","cite"] $ do
  (url, _) <- runForm "cite" displayJsonForm citeForm
  cite <- maybeAction =<< lookupCitation url
  okResponse [] $ toJSON cite
