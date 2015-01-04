{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module App where

import Control.Monad.Reader (local)
import Control.Monad.State (get)
import Control.Lens (makeLenses, set)
import qualified Database.PostgreSQL.Typed as PG
import qualified Language.Haskell.TH as TH
import Snap.Snaplet (Snaplet, Handler, snapletValue, with)
import Snap.Snaplet.PostgresqlTyped (PG, HasPG(..), loadPGDatabase)

data App = App
  { _db :: Snaplet PG
  }

makeLenses ''App

instance HasPG (Handler b App) where
  getPGState = with db get
  setLocalPGState s = local (set (db . snapletValue) s)

type AppHandler = Handler App App

-- only do it once?
useTPG :: TH.DecsQ
useTPG = PG.useTPGDatabase =<< TH.runIO (loadPGDatabase "snaplets/postgresql-typed/devel.cfg")
