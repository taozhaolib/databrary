{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Audit
  ( viewActivity
  ) where

import Control.Arrow (second)
import Data.Ord (comparing)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.VolumeAccess
import Databrary.Model.Authorize
import Databrary.Model.Party
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Volume

viewActivity :: AppRoute ()
viewActivity = action GET (pathJSON >/> "activity") $ \() -> withAuth $ do
  vl <- mapM (liftK second vol) =<< lookupVolumeActivity 8
  al <- map (second $ ("party" JSON..=) . partyJSON) <$> lookupAuthorizeActivity 8
  okResponse [] $ JSON.toJSON $ map ent $ take 12 $ mergeBy (comparing fst) vl al
  where
  vol v = ("volume" JSON..=) <$> volumeJSONQuery v [("access", Just "ADMIN")]
  ent (t, j) = JSON.object ["time" JSON..= t, j]
