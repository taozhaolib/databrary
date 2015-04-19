{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Web
  ( webFile
  ) where

import qualified Data.Text as T

import Databrary.Action.Route
import Databrary.Action
import Databrary.HTTP.File
import Databrary.Web.Files (webDir)
import Databrary.Web.Rules

webFile :: StaticPath -> AppRAction
webFile sp = action GET ("public" :: T.Text, sp) $ do
  _ <- generateWebFile (staticFilePath sp)
  serveStaticFile webDir sp
