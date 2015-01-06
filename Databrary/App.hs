{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Databrary.App where

import Control.Monad.Reader (local)
import Control.Monad.State (get)
import Control.Lens (makeLenses, set)
import Snap.Snaplet (Snaplet, Handler, snapletValue, with)

import Databrary.Snaplet.PG (PG, HasPG(..))

data App = App
  { _db :: Snaplet PG
  }

makeLenses ''App

instance HasPG (Handler b App) where
  getPGState = with db get
  setLocalPGState s = local (set (db . snapletValue) s)

type AppHandler = Handler App App
