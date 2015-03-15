{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Angular
  ( jsURL
  , angular
  , angularConstants
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types (encodePath, hUserAgent, hContentType)
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Has (peeks)
import qualified Databrary.JSON as JSON
import Databrary.Model.Enum
import Databrary.Model.Permission.Types
import Databrary.Model.Consent.Types
import Databrary.Model.Metric
import Databrary.Model.RecordCategory
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Action
import Databrary.Web.Request
import Databrary.View.Angular

jsURL :: Maybe Bool -> Wai.Request -> (Maybe Bool, BSB.Builder)
jsURL js req =
  second (encodePath (Wai.pathInfo req) . maybe id (\v -> (("js", Just (if v then "1" else "0")) :)) js)
  $ unjs $ Wai.queryString req where
  unjs [] = (Nothing, [])
  unjs (("js",v):q) = (Just (bool v), snd $ unjs q)
  unjs (x:q) = second (x:) $ unjs q
  bool (Just "0") = False
  bool (Just "false") = False
  bool (Just "off") = False
  bool (Just "") = False
  bool _ = True

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnabled :: Wai.Request -> Bool
angularEnabled req = fromMaybe def $ fst $ jsURL Nothing req where
  def = not $ Fold.any (Regex.matchTest browserBlacklist) $ lookupRequestHeader hUserAgent req

viewAngular :: MonadAuthAction q m => m Response
viewAngular =
  okResponse [] =<< peeks htmlAngular 

angular :: (MonadIO m, MonadAuthAction q m) => m ()
angular = do
  js <- peeks angularEnabled
  when js $ result =<< viewAngular

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
