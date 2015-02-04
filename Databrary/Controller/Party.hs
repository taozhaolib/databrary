{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( getParty
  ) where

import Control.Has (see, peeks)
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Party

getParty :: Bool -> Id Party -> AppRAction
getParty api i = action GET (apiRoute api $ toRoute i) $ withAuth $ do
  p <- maybeAction =<< lookupAuthParty i
  if api
    then okResponse [] =<< peeks (authPartyJSON p)
    else okResponse [] (partyName $ see p)
