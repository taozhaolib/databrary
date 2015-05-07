{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Types
  ( Service(..)
  , MonadHasService
  ) where

import qualified Data.ByteString as BS

import Databrary.Has (makeHasRec)
import Databrary.Service.DB (DBConn)
import Databrary.Service.Entropy (Entropy)
import Databrary.HTTP.Client (HTTPClient)
import Databrary.Store.Types (Storage)
import Databrary.Service.Passwd (Passwd)
import Databrary.Service.Log (Logs)
import Databrary.Service.Messages (Messages)
import Databrary.Web.Types (Web)
import Databrary.Media.AV (AV)
import Databrary.Model.Time

data Service = Service
  { serviceStartTime :: !Timestamp
  , serviceLogs :: !Logs
  , serviceSecret :: !BS.ByteString
  , serviceMessages :: !Messages
  , serviceEntropy :: !Entropy
  , servicePasswd :: !Passwd
  , serviceDB :: !DBConn
  , serviceStorage :: !Storage
  , serviceWeb :: !Web
  , serviceAV :: !AV
  , serviceHTTPClient :: !HTTPClient
  }

makeHasRec ''Service ['serviceDB, 'serviceMessages, 'serviceEntropy, 'serviceHTTPClient, 'serviceStorage, 'serviceWeb, 'servicePasswd, 'serviceAV]
