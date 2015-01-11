{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Databrary.App where

import Control.Lens (makeLenses)
import Snap.Snaplet (Snaplet, Handler)

import Databrary.Snaplet.PG (PG, HasPG(..))
import Databrary.Snaplet.Entropy (Entropy, HasEntropy(..))

data App = App
  { _db :: Snaplet PG
  , _entropy :: Snaplet Entropy
  }

makeLenses ''App

instance HasPG App where
  pgLens = db
instance HasEntropy App where
  entropyLens = entropy

type AppHandler = Handler App App
