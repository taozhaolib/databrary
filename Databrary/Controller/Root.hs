{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Root
  ( viewRoot
  , viewConstants
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON

import Databrary.Has (peek)
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root
import Databrary.Web.Constants

viewRoot :: AppRoute API
viewRoot = action GET pathAPI $ \api -> withAuth $ do
  when (api == HTML) angular
  case api of
    JSON -> okResponse [] JSON.emptyObject
    HTML -> okResponse [] . htmlRoot =<< peek

viewConstants :: AppRoute ()
viewConstants = action GET (pathJSON >/> "constants") $ \() ->
  okResponse [] constantsJSON
