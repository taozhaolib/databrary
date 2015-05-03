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
  [ AnyRoute viewRoot

  , AnyRoute viewUser
  , AnyRoute postUser
  , AnyRoute viewLogin
  , AnyRoute postLogin
  , AnyRoute postLogout
  , AnyRoute viewRegister
  , AnyRoute postRegister
  , AnyRoute viewPasswordReset
  , AnyRoute postPasswordReset
  , AnyRoute viewLoginToken
  , AnyRoute postPasswordToken

  , AnyRoute viewParty
  , AnyRoute postParty
  , AnyRoute viewEditParty
  , AnyRoute viewAuthorize
  , AnyRoute postAuthorize
  , AnyRoute deleteAuthorize
  , AnyRoute viewAvatar
  , AnyRoute createParty
  , AnyRoute queryParties

  , AnyRoute viewVolume
  , AnyRoute postVolume
  , AnyRoute viewVolumeForm
  , AnyRoute viewVolumeAccess
  , AnyRoute postVolumeAccess
  , AnyRoute viewVolumeLinks
  , AnyRoute postVolumeLinks
  , AnyRoute postVolumeFunding
  , AnyRoute deleteVolumeFunder
  , AnyRoute createVolume
  , AnyRoute queryVolumes

  , AnyRoute createContainer
  , AnyRoute viewSlot
  , AnyRoute postContainer

  , AnyRoute viewAsset
  , AnyRoute postAsset
  , AnyRoute viewEditAsset
  , AnyRoute deleteAsset
  , AnyRoute downloadAsset
  , AnyRoute viewCreateAsset
  , AnyRoute createAsset
  , AnyRoute createSlotAsset
  , AnyRoute viewCreateSlotAsset

  , AnyRoute viewAssetSegment
  , AnyRoute downloadAssetSegment
  , AnyRoute thumbAssetSegment
  , AnyRoute postExcerpt
  , AnyRoute deleteExcerpt

  , AnyRoute createRecord
  , AnyRoute viewRecord
  , AnyRoute postRecordMeasure

  , AnyRoute postTag
  , AnyRoute deleteTag
  , AnyRoute postComment

  , AnyRoute uploadStart
  , AnyRoute uploadChunk
  , AnyRoute testChunk
  , AnyRoute viewConstants
  , AnyRoute getCitation
  , AnyRoute queryFunder
  , AnyRoute remoteTranscode

  , AnyRoute webFile
  ]

{-
    if actionMethod ra == methodGet && Wai.rawPathInfo req /= actionRoute ra
      then emptyResponse movedPermanently301 [(hLocation, actionURL ra (Just req) `BS.append` Wai.rawQueryString req)]
      else routeAction ra
-}
