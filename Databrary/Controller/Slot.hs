{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Slot
  ( getSlot
  ) where

import Databrary.Model.Permission
import Databrary.Model.Slot

getSlot :: Permission -> Id Container -> Segment -> AuthActionM Slot
getSlot p i s =
  checkPermission p =<< maybeAction =<< lookupSlot i s
