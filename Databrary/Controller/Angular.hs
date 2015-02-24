{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Angular
  ( angularEnabled
  , angularConstants
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Fold
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types (hUserAgent, hContentType)
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import qualified Databrary.JSON as JSON
import Databrary.Model.Enum
import Databrary.Model.Permission.Types
import Databrary.Model.Metric
import Databrary.Model.RecordCategory
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Action
import Databrary.Web.Request

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnabled :: Wai.Request -> Bool
angularEnabled req = getjs $ lookupQueryParameters "js" req where
  getjs ((Just "0"):_) = False
  getjs ((Just "false"):_) = False
  getjs ((Just "off"):_) = False
  getjs ((Just ""):_) = False
  getjs (_:_) = True
  getjs [] = not $ Fold.any (Regex.matchTest browserBlacklist) $ lookupRequestHeader hUserAgent req

constantsJS :: BSL.ByteString
constantsJS =
  "app.constant('constantData'," <> JSON.encode constantsJSON <> ");"
  where
  enumValues :: forall a . DBEnum a => a -> [String]
  enumValues _ = map show $ enumFromTo minBound (maxBound :: a)
  constantsJSON = JSON.object
    [ "permission" JSON..= enumValues PermissionPUBLIC
    , "consent" JSON..= enumValues ConsentPUBLIC
    , "classification" JSON..= enumValues ClassificationPUBLIC
    , "metric" JSON..= JSON.recordMap (map metricJSON allMetrics)
    , "category" JSON..= JSON.recordMap (map recordCategoryJSON allRecordCategories)
    , "format" JSON..= JSON.recordMap (map formatJSON allFormats)
    , "party" JSON..= JSON.object
      [ "nobody" JSON..= partyJSON nobodyParty
      , "root" JSON..= partyJSON rootParty
      ]
    -- TODO: mode? url? version?
    ]

angularConstants :: AppRAction
angularConstants = action GET ("constants.js" :: T.Text) $ do
  okResponse [(hContentType, "text/javascript")] constantsJS
