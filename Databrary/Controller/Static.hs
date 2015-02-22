module Databrary.Controller.Static
  ( staticPublicFile
  ) where

import Databrary.Action.Route
import Databrary.Action
import Databrary.Web.File

staticPublicFile :: StaticPath -> AppRAction
staticPublicFile sp = action GET (toRoute sp) $ do
  serveStaticFile "public" sp
