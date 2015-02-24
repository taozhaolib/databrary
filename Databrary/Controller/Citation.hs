{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)
import qualified Data.Text as T
import Network.URI (URI)

import Databrary.Action
import Databrary.Web.Form.Deform
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

citeForm :: (Functor m, Monad m) => DeformT m URI
citeForm = "url" .:> deform

getCitation :: AppRAction
getCitation = action GET ["api" :: T.Text,"cite"] $ do
  url <- runForm Nothing citeForm
  cite <- maybeAction =<< lookupCitation url
  okResponse [] $ toJSON cite
