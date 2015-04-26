{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import qualified Data.ByteString as BS
import Control.Applicative ((<|>))
import Control.Monad (msum, guard)
import Control.Monad.Reader (ask)
import Network.HTTP.Types (methodGet, movedPermanently301, hLocation)
import qualified Network.Wai as Wai

import qualified Databrary.HTTP.Route as R
import Databrary.Action
import Databrary.Action.Types
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

act :: ActionData q => RouteAction q -> R.RouteM (Action q)
act ra = do
  R.final
  req <- ask
  guard $ actionMethod ra == Wai.requestMethod req
  return $
    if actionMethod ra == methodGet && Wai.rawPathInfo req /= actionRoute ra
      then emptyResponse movedPermanently301 [(hLocation, actionURL ra (Just req) `BS.append` Wai.rawQueryString req)]
      else routeAction ra

routes :: R.RouteM AppAction
routes = do
  api <- R.route
  let
    json = guard (api == JSON)
    html = guard (api == HTML)
  msum 
    [                                 act (viewRoot api)
    , "user" >> msum                  -- /user
      [                      (html >> act viewUser)
      ,                               act (postUser api)
      , "login" >>           (html >> act viewLogin)
                                  <|> act (postLogin api)
      , "logout" >>                   act (postLogout api)
      , "register" >>        (html >> act viewRegister)
                                  <|> act (postRegister api)
      , "password" >>        (html >> act viewPasswordReset)
                                  <|> act (postPasswordReset api)
      ]
    , R.route >>= \t ->               -- /token/ID
                                      act (viewLoginToken api t)
                                  <|> act (postPasswordToken api t)
    , R.route >>= \p -> msum          -- /party/ID
      [                               act (viewParty api p)
      ,                               act (postParty api p)
      , html >> "edit" >>             act (viewEditParty p)
      , R.route >>= \a -> msum        --          /authorize/ID
        [                             act (viewAuthorize api p a)
        ,                             act (postAuthorize api p a)
        ,                             act (deleteAuthorize api p a)
        ]
      ]
    , R.route >>= \p ->
                  html >> "avatar" >> act (viewAvatar p)
    , "party" >>                      act (createParty api)
                                  <|> act (queryParties api)
    , R.route >>= \v -> msum          -- /vo/ume/ID
      [                               act (viewVolume api v)
      ,                               act (postVolume api v)
      , html >> "edit" >>             act (viewVolumeForm v)
      , R.route >>= \p ->             --           /access/ID
                             (html >> act (viewVolumeAccess v p))
                                  <|> act (postVolumeAccess api v p)
      , "link" >>            (html >> act (viewVolumeLinks v))
                                  <|> act (postVolumeLinks api v)
      , R.route >>= \f ->     json >> --           /funder/ID
                                      act (postVolumeFunding v f)
                                  <|> act (deleteVolumeFunder v f)
      , "slot" >>                     act (createContainer api v)
      , "record" >>                   act (createRecord api v)
      , "asset" >>           (html >> act (viewCreateAsset v))
                                  <|> act (createAsset api v)
      , json >> "upload" >>           act (uploadStart v)
      ]
    , "volume" >>                     act (createVolume api)
                                  <|> act (queryVolumes api)

    , R.route >>= \s -> msum          -- /slot/ID/SEG
      [                               act (viewSlot api s)
      ,                               act (postContainer api s)
      , R.route >>= \a -> msum        --             /asset/ID
        [                             act (viewAssetSegment api s a)
        , html >> "download" >>       act (downloadAssetSegment s a)
        , html >> "thumb" >>          act (thumbAssetSegment s a)
        , "excerpt" >>        json >> act (postExcerpt s a)
                                  <|> act (deleteExcerpt s a)
        ]
      , "asset" >>           (html >> act (viewCreateSlotAsset s))
                                  <|> act (createSlotAsset api s)
      , R.route >>= \t ->             act (postTag api s t)
                                  <|> act (deleteTag api s t)
      , "comment" >>                  act (postComment api s)
      ]

    , R.route >>= \r -> msum          -- /record/ID
      [                               act (viewRecord api r)
      , R.route >>= \m ->             act (postRecordMeasure api r m)
      ]

    , R.route >>= \a -> msum          -- /asset/ID
      [                               act (viewAsset api a)
      ,                               act (postAsset api a)
      , html >> "edit" >>             act (viewEditAsset a)
      ,                               act (deleteAsset api a)
      , html >> "download" >>         act (downloadAsset a)
      ]

    , json >> msum                    -- /api
      [ "constants" >>                act viewConstants
      , "cite" >>                     act getCitation
      , "upload" >>                   act uploadChunk
                                  <|> act testChunk
      , "funder" >>                   act queryFunder
      , R.route >>= \t ->             act (remoteTranscode t)
      ]

    , html >> msum                    -- /
      [ "public" >> R.route >>=       act . webFile
      ]
    ]
