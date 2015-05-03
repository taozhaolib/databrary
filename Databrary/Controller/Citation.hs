{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)

import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

getCitation :: AppRoute ()
getCitation = action GET (pathJSON </< "cite") $ \() -> do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< lookupCitation url
  okResponse [] $ toJSON cite
