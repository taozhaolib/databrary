{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Databrary.Controller.Analytics
  ( angularAnalytics
  ) where

import Control.Monad (when)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Vector as V

import Control.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Model.Audit
import Databrary.Web.Request

angularAnalytics :: AuditM q m => m ()
angularAnalytics = do
  req <- peek
  when (Fold.any ("DatabraryClient" ==) $ lookupRequestHeader "x-requested-with" req) $
    mapM_ auditAnalytic $ pr . P.parseOnly JSON.json' =<< lookupRequestHeaders "analytics" req
  where
  pr (Left _) = []
  pr (Right (JSON.Array l)) = mapMaybe ar $ V.toList l
  pr (Right j) = maybeToList $ ar j
  ar (JSON.Object o)
    | Just a <- JSON.parseMaybe JSON.parseJSON =<< HM.lookup "action" o
    , Just r <- JSON.parseMaybe JSON.parseJSON =<< HM.lookup "route" o
    = Just $ Analytic a r $ JSON.Object $ HM.delete "action" $ HM.delete "route" o
  ar _ = Nothing
