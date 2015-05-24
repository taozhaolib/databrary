{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routeMap
  , jsRoutes
  ) where

import qualified Data.ByteString.Builder as B
import Data.Monoid (mconcat)

import Databrary.HTTP.Route
import Databrary.Model.Id.Types
import Databrary.Model.Slot.Types
import Databrary.Action
import Databrary.Controller.Paths
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
import Databrary.Controller.Format
import Databrary.Controller.Asset
import Databrary.Controller.AssetSegment
import Databrary.Controller.Excerpt
import Databrary.Controller.Zip
import Databrary.Controller.Tag
import Databrary.Controller.Comment
import Databrary.Controller.Transcode
import Databrary.Controller.Web
import Databrary.Web.Routes

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
  , route viewPartyEdit
  , route viewAuthorize
  , route postAuthorize
  , route deleteAuthorize
  , route viewAvatar
  , route createParty
  , route queryParties

  , route viewVolume
  , route postVolume
  , route viewVolumeEdit
  , route viewVolumeAccess
  , route postVolumeAccess
  , route viewVolumeLinks
  , route postVolumeLinks
  , route postVolumeFunding
  , route deleteVolumeFunder
  , route viewVolumeCreate
  , route createVolume
  , route queryVolumes
  , route zipVolume

  , route createContainer
  , route viewSlot
  , route viewContainerEdit
  , route postContainer
  , route zipContainer

  , route viewFormats

  , route viewAsset
  , route postAsset
  , route viewAssetEdit
  , route deleteAsset
  , route downloadAsset
  , route viewAssetCreate
  , route createAsset
  , route createSlotAsset
  , route viewSlotAssetCreate

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

jsRoutes :: B.Builder
jsRoutes = mconcat
  [ jsRoute "viewRoot" viewRoot HTML
  , jsRoute "viewLogin" viewLogin ()
  , jsRoute "viewRegister" viewRegister ()
  , jsRoute "viewPasswordReset" viewPasswordReset ()
  , jsRoute "viewLoginToken" viewLoginToken (HTML, token)

  , jsRoute "viewProfile" viewParty (HTML, TargetProfile)
  , jsRoute "viewParty" viewParty (HTML, TargetParty party)
  , jsRoute "viewPartyEdit" viewPartyEdit (TargetParty party)
  , jsRoute "viewPartySearch" queryParties HTML
  , jsRoute "partyAvatar" viewAvatar party

  , jsRoute "viewVolume" viewVolume (HTML, volume)
  , jsRoute "viewVolumeCreate" viewVolumeCreate ()
  , jsRoute "viewVolumeEdit" viewVolumeEdit volume
  , jsRoute "viewVolumeSearch" queryVolumes HTML

  , jsRoute "viewSlot" viewSlot (HTML, slot)
  , jsRoute "viewSlotEdit" viewContainerEdit slot

  , jsRoute "viewRecord" viewRecord (HTML, record)

  , jsRoute "viewFormats" viewFormats ()
  , jsRoute "viewAssetSegment" viewAssetSegment (HTML, slot, asset)
  , jsRoute "viewAssetEdit" viewAssetEdit asset
  , jsRoute "downloadAssetSegment" downloadAssetSegment (slot, asset)
  , jsRoute "thumbAssetSegment" thumbAssetSegment (slot, asset)

  , jsRoute "zipSlot" zipContainer slot
  , jsRoute "zipVolume" zipVolume volume
  ] where
  token = Id ""
  party = Id 0
  volume = Id 0
  slot = containerSlotId (Id 0)
  asset = Id 0
  record = Id 0
