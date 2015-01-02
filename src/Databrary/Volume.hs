{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Databrary.Volume where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32)
import Snap.Core (writeBS)
import Snap.Snaplet.TemplatePG (HasPG, pgQuery, pgSQL)

import App

useTPG

newtype VolumeId = VolumeId Int32 deriving (Eq) -- PGType?

data Volume = Volume
  { volumeId :: VolumeId
  , volumeName :: String -- ByteString?
  , volumeAlias :: Maybe String
  }

getVolume :: HasPG m => VolumeId -> m (Maybe Volume)
getVolume (VolumeId i) = do
  r <- pgQuery [pgSQL|SELECT id, name, alias FROM volume WHERE id = ${i}|]
  case r of
    [] -> return Nothing
    [(i, n, a)] -> return $ Just $ Volume (VolumeId i) n a
    _ -> fail "multiple"

volume :: Int -> AppHandler ()
volume vi = do
  v <- getVolume (VolumeId (fromIntegral vi))
  writeBS $ maybe "not found" (BSC.pack . volumeName) v
