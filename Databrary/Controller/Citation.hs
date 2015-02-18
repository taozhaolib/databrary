{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)
import Network.URI (URI)

import Databrary.Action
import Databrary.Web.Deform
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

citeForm :: (Functor m, Monad m) => DeformT m URI
citeForm = "url" .:> deform

getCitation :: AppRAction
getCitation = action GET ["api","cite"] $ do
  url <- runForm Nothing citeForm
  cite <- maybeAction =<< lookupCitation url
  okResponse [] $ toJSON cite
