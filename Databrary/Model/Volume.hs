{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Volume where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Snap.Core (writeText)

import Databrary.App
import Databrary.Snaplet.PG
import Databrary.Model.Id

useTPG

data Volume = Volume
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeAlias :: Maybe T.Text
  }

instance HasId Volume where
  key = volumeId
  kind _ = "volume"

getVolume :: (Functor m, Monad m, HasPG m) => Id Volume -> m (Maybe Volume)
getVolume k =
  fmap (\(i, n, a) -> Volume i n a)
    <$> pgQuery1 [pgSQL|SELECT id, name, alias FROM volume WHERE id = ${k}|]

volume :: Id Volume -> AppHandler ()
volume vk = do
  v <- getVolume vk
  writeText $ maybe "not found" volumeName v
