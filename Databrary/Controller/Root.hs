module Databrary.Controller.Root
  ( viewRoot
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON

import Control.Has (peek)
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root

viewRoot :: API -> AppRAction
viewRoot api = action GET api $ withAuth $ do
  when (api == HTML) angular
  case api of
    JSON -> okResponse [] JSON.emptyObject
    HTML -> okResponse [] . htmlRoot =<< peek
