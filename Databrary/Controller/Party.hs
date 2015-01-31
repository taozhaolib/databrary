{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( getParty
  ) where

import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Party

getParty :: Bool -> Id Party -> AppRAction
getParty api i = action GET (apiRoute api $ toRoute i) $ do
  p <- lookupParty i
  okResponse [] (maybe "not found" partyName p)
