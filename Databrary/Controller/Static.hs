{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Static
  ( staticPath
  , staticPublicFile
  ) where

import qualified Data.Text as T

import Databrary.Action.Route
import Databrary.Action
import Databrary.Web.File

staticPublicFile :: StaticPath -> AppRAction
staticPublicFile sp = action GET ("public" :: T.Text, sp) $ do
  serveStaticFile "public" sp
