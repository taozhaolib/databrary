{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module Databrary.Snaplet.Entropy
  ( Entropy
  , HasEntropy(..)
  , entropyInit
  , entropyBytes
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.ByteString as BS
import Snap.Snaplet
import qualified System.Entropy as Entropy

newtype Entropy = Entropy Entropy.CryptHandle

class HasEntropy b where
  entropyLens :: SnapletLens b Entropy

entropyInit :: SnapletInit b Entropy
entropyInit = makeSnaplet "Entroy" "System.Entropy" Nothing $
  liftIO $ Entropy <$> Entropy.openHandle

entropyBytes :: HasEntropy b => Int -> Handler b b BS.ByteString
entropyBytes n = with entropyLens $ do
  Entropy h <- ask
  liftIO $ Entropy.hGetEntropy h n

