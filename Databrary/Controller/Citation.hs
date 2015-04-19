{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)
import qualified Data.Text as T

import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

getCitation :: AppRAction
getCitation = action GET (JSON, "cite" :: T.Text) $ do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< lookupCitation url
  okResponse [] $ toJSON cite
