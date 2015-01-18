{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Volume 
  ( testVolume
  ) where

import Control.Applicative ((<$>))
import qualified Data.Text as T

import Databrary.App
import Databrary.Snaplet.PG
import Databrary.Model.Id

useTPG

data Volume = Volume
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeAlias :: Maybe T.Text
  , volumeBody :: Maybe T.Text
--  , volumeCreation :: Timestamp
  }

instance HasId Volume where
  idOf = volumeId
  kindOf _ = "volume"

class InVolume a where
  volume :: a -> Volume

instance InVolume Volume where
  volume = id

getVolume :: Id Volume -> AppHandler (Maybe Volume)
getVolume k =
  fmap (uncurryN Volume)
    <$> pgQuery1 [pgSQL|SELECT id, name, alias, body FROM volume WHERE id = ${k}|]


testVolume :: Id Volume -> AppHandler ()
testVolume vk = do
  v <- getVolume vk
  writeText $ maybe "not found" volumeName v
