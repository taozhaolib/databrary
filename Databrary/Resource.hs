{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, DefaultSignatures, TypeFamilies #-}
module Databrary.Resource
  ( Resource(..)
  , ResourceT
  , HasResource(..)
  , initResource
  , runResource
  ) where

import Control.Monad (liftM3)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Databrary.Resource.DB
import Databrary.Resource.Entropy
import Databrary.Web.Wai (WaiT)

data Resource = Resource
  { resourceConfig :: C.Config
  , resourceSecret :: BS.ByteString
  , resourceDB :: DBConn
  , resourceEntropy :: Entropy
  }

initResource :: IO Resource
initResource = do
  conf <- C.load [C.Required "databrary.conf"]
  liftM3 (Resource conf)
    (C.require conf "secret")
    (initDB $ C.subconfig "db" conf)
    initEntropy

type ResourceT = ReaderT Resource

class Monad m => HasResource m where
  getResource :: (Resource -> a) -> m a
  default getResource :: (MonadTrans t, HasResource b, m ~ t b) => (Resource -> a) -> t b a
  getResource = lift . getResource

instance Monad m => HasResource (ResourceT m) where
  getResource = asks
instance HasResource m => HasResource (WaiT r m)

runResource :: ResourceT m a -> Resource -> m a
runResource = runReaderT
