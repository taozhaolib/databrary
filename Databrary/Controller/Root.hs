module Databrary.Controller.Root
  ( viewRoot
  ) where

import qualified Data.Aeson.Types as JSON

import Control.Has (peek)
import Databrary.Action
import Databrary.View.Root

viewRoot :: Bool -> AppRAction
viewRoot api = action GET [] $ withAuth $
  if api
    then okResponse [] JSON.emptyObject
    else okResponse [] . htmlRoot =<< peek
