{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Container
  ( htmlContainerForm
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Container

htmlContainerForm :: Either Volume Container -> AuthRequest -> FormHtml
htmlContainerForm targ req = htmlForm
  (either (const "Create container") (("Edit container " <>) . fromMaybe "" . containerName) targ)
  (either (createContainer HTML . volumeId) (postContainer HTML . containerSlotId . containerId) targ) req
  $ do
  field "name" $ inputText (containerName =<< cont)
  field "date" $ inputDate (containerDate =<< cont)
  field "release" $ inputEnum (containerRelease =<< cont)
  where
  cont = either (const Nothing) Just targ
