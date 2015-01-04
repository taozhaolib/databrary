{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Databrary.Volume where

import Data.Int (Int32)
import qualified Data.Text as T
import Snap.Core (writeText)
import Snap.Snaplet.PostgresqlTyped (HasPG, pgQuery, pgSQL)

import App

useTPG

newtype VolumeId = VolumeId Int32 deriving (Eq) -- PGType?

data Volume = Volume
  { volumeId :: VolumeId
  , volumeName :: T.Text
  , volumeAlias :: Maybe T.Text
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
  writeText $ maybe "not found" volumeName v
