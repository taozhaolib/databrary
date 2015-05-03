{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routeMap
  ) where

import Databrary.HTTP.Route
import Databrary.Action
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Register
import Databrary.Controller.Token
import Databrary.Controller.Party
import Databrary.Controller.Authorize
import Databrary.Controller.Volume
import Databrary.Controller.VolumeAccess
import Databrary.Controller.Funding
import Databrary.Controller.Container
import Databrary.Controller.Slot
import Databrary.Controller.Record
import Databrary.Controller.Citation
import Databrary.Controller.Upload
import Databrary.Controller.Asset
import Databrary.Controller.AssetSegment
import Databrary.Controller.Excerpt
import Databrary.Controller.Tag
import Databrary.Controller.Comment
import Databrary.Controller.Transcode
import Databrary.Controller.Web

routeMap :: RouteMap AppAction
routeMap = fromRouteList
  [ route viewRoot

  , route viewUser
  , route postUser
  , route viewLogin
  , route postLogin
  , route postLogout
  , route viewRegister
  , route postRegister
  , route viewPasswordReset
  , route postPasswordReset
  , route viewLoginToken
  , route postPasswordToken

  , route viewParty
  , route postParty
  , route viewEditParty
  , route viewAuthorize
  , route postAuthorize
  , route deleteAuthorize
  , route viewAvatar
  , route createParty
  , route queryParties

  , route viewVolume
  , route postVolume
  , route viewVolumeForm
  , route viewVolumeAccess
  , route postVolumeAccess
  , route viewVolumeLinks
  , route postVolumeLinks
  , route postVolumeFunding
  , route deleteVolumeFunder
  , route createVolume
  , route queryVolumes

  , route createContainer
  , route viewSlot
  , route postContainer

  , route viewAsset
  , route postAsset
  , route viewEditAsset
  , route deleteAsset
  , route downloadAsset
  , route viewCreateAsset
  , route createAsset
  , route createSlotAsset
  , route viewCreateSlotAsset

  , route viewAssetSegment
  , route downloadAssetSegment
  , route thumbAssetSegment
  , route postExcerpt
  , route deleteExcerpt

  , route createRecord
  , route viewRecord
  , route postRecordMeasure

  , route postTag
  , route deleteTag
  , route postComment

  , route uploadStart
  , route uploadChunk
  , route testChunk
  , route viewConstants
  , route getCitation
  , route queryFunder
  , route remoteTranscode

  , route webFile
  ]

{-
    if actionMethod ra == methodGet && Wai.rawPathInfo req /= actionRoute ra
      then emptyResponse movedPermanently301 [(hLocation, actionURL ra (Just req) `BS.append` Wai.rawQueryString req)]
      else routeAction ra
-}
