{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Volume
  ( htmlVolumeForm
  -- , htmlVolumeSearchForm
  ) where

import qualified Data.Text as T

import Control.Applicative.Ops
import Databrary.Action.Auth
import Databrary.Action
import Databrary.View.Form
import Databrary.Model.Volume

import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeForm :: Maybe Volume -> AuthRequest -> FormHtml
htmlVolumeForm v req = htmlForm (maybe "Create volume" ((T.append "Edit ") . volumeName) v) (maybe (createVolume HTML) (postVolume HTML . volumeId) v) req $ do
  field "name" $ inputText $ volumeName <$> v
  field "alias" $ inputText $ volumeAlias =<< v
  field "body" $ inputTextarea $ volumeBody =<< v

{-
htmlVolumeSearchForm :: VolumeFilter -> AuthRequest -> FormHtml
htmlVolumeSearchForm vf req = htmlForm "Search volumes" (searchVolume HTML) req $ do
  field "query" $ inputText $ volumeFilterQuery vf
  -}
