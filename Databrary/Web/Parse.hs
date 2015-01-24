{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Parse
  ( limitBackEnd
  , DynamicContent(..)
  , dynamicBackEnd
  ) where

import Control.Applicative ((<$>))
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Network.Wai.Parse

mapBackEnd :: (a -> b) -> BackEnd a -> BackEnd b
mapBackEnd f back param info next = f <$> back param info next

limitBackEnd :: Word64 -> BackEnd a -> BackEnd (Either Word64 a)
limitBackEnd lim back param info next = do
  len <- newIORef 0
  let go n = do
        b <- next
        let n' = n + fromIntegral (BS.length b)
        if BS.null b || n' <= lim
          then do
            writeIORef len n'
            return b
          else go n'
  r <- back param info (go =<< readIORef len)
  n <- readIORef len
  return $ if n <= lim then Right r else Left n

parserBackEnd :: AP.Parser a -> BackEnd (AP.Result a)
parserBackEnd parser _ _ next = run (AP.parse parser) where
  run p = do
    b <- next
    let r = p b
    if BS.null b
      then return r
      else run $ AP.feed r

jsonBackEnd :: BackEnd (AP.Result JSON.Value)
jsonBackEnd = parserBackEnd JSON.json

data DynamicContent
  = DynamicMemory LBS.ByteString
  -- | DynamicFile FilePath
  | DynamicJSON (Maybe JSON.Value)
  | DynamicRejected Word64

dynamicBackEnd :: Word64 -> BackEnd DynamicContent
dynamicBackEnd lim = 
  mapBackEnd (either DynamicRejected id) $ limitBackEnd lim back where
  back param info@FileInfo{ fileContentType = "text/json" } =
    mapBackEnd (DynamicJSON . AP.maybeResult) jsonBackEnd param info
  back param info =
    mapBackEnd DynamicMemory lbsBackEnd param info
