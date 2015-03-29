{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Volume
  ( htmlVolumeForm
  , htmlVolumeLinksForm
  , htmlVolumeSearchForm
  ) where

import qualified Data.Text as T

import Databrary.Ops
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Citation
import Databrary.Web.Form.View
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeForm :: Maybe Volume -> Maybe Citation -> AuthRequest -> FormHtml
htmlVolumeForm vol cite req = htmlForm (maybe "Create volume" ((T.append "Edit ") . volumeName) vol) (maybe (createVolume HTML) (postVolume HTML . volumeId) vol) req $ do
  field "name" $ inputText $ volumeName <$> vol
  field "alias" $ inputText $ volumeAlias =<< vol
  field "body" $ inputTextarea $ volumeBody =<< vol
  "citation" .:> do
    field "head" $ inputText $ citationHead <$> cite
    field "url" $ inputText $ fmap show $ citationURL =<< cite
    field "year" $ inputText $ fmap show $ citationYear =<< cite

htmlVolumeLinksForm :: Volume -> [Citation] -> AuthRequest -> FormHtml
htmlVolumeLinksForm vol links req = htmlForm "Edit volume links" (postVolumeLinks HTML (volumeId vol)) req $
  withSubFormsViews links $ \link -> do
    field "head" $ inputText $ citationHead <$> link
    field "url" $ inputText $ fmap show $ citationURL =<< link

htmlVolumeSearchForm :: VolumeFilter -> AuthRequest -> FormHtml
htmlVolumeSearchForm vf req = htmlForm "Search volumes" (queryVolumes HTML) req $ do
  field "query" $ inputText $ volumeFilterQuery vf
