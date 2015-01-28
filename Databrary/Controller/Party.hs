{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( getParty
  ) where

import Network.HTTP.Types (ok200)

import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Party

getParty :: Bool -> Id Party -> AppRAction
getParty api i = bAction GET (apiRoute api $ toRoute i) $ do
  p <- lookupParty i
  respond $ maybe "not found" partyName p
  return ok200

