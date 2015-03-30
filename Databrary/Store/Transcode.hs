module Databrary.Store.Transcode
  ( 
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Databrary.Model.Transcode.Types
import Databrary.Store.Types
import Databrary.Store.Transcoder

{-
startTranscode :: DBM m => Transcode -> m TranscodePID
startTranscode tc = do
  tc' <- updateTranscode tc lock Nothing
  unless (tronscodePID tc' == lock) $ fail $ "startTranscode " ++ show (assetId (transcodeAsset tc))
  where lock = Just -1
        -}
