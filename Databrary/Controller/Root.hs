module Databrary.Controller.Root
  ( viewRoot
  ) where

import qualified Data.Aeson.Types as JSON

import Control.Has (peek)
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root

viewRoot :: API -> AppRAction
viewRoot api = action GET api $ withAuth $
  case api of
    JSON -> okResponse [] JSON.emptyObject
    HTML -> angular $ okResponse [] . htmlRoot =<< peek
