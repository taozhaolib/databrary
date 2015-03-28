{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Token
  ( module Databrary.Model.Token.Types
  , loginTokenId
  , lookupLoginToken
  , createLoginToken
  , removeLoginToken
  , lookupSession
  , createSession
  , removeSession
  , lookupUpload
  , createUpload
  , removeUpload
  ) where

import Control.Monad (when, void, (<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import Data.Int (Int64)
import Database.PostgreSQL.Typed (pgSQL)
import System.Posix.Files.ByteString (removeLink)

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import Databrary.Resource
import Databrary.Entropy
import Databrary.Crypto
import Databrary.DB
import Databrary.Store.Storage
import Databrary.Store.Upload
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Offset
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party
import Databrary.Model.Token.Types
import Databrary.Model.Token.SQL

useTPG

loginTokenId :: (MonadHasResource c m, EntropyM c m) => LoginToken -> m (Id LoginToken)
loginTokenId tok = Id <$> sign (unId (view tok :: Id Token))

lookupLoginToken :: (DBM m, MonadHasResource c m) => Id LoginToken -> m (Maybe LoginToken)
lookupLoginToken =
  flatMapM (\t -> dbQuery1 $(selectQuery selectLoginToken "$!WHERE login_token.token = ${t} AND expires > CURRENT_TIMESTAMP"))
    <=< unSign . unId

lookupSession :: DBM m => BS.ByteString -> m (Maybe Session)
lookupSession tok =
  dbQuery1 $(selectQuery selectSession "$!WHERE session.token = ${tok} AND expires > CURRENT_TIMESTAMP")

lookupUpload :: (DBM m, MonadHasIdentity c m) => BS.ByteString -> m (Maybe Upload)
lookupUpload tok = do
  auth <- peek
  dbQuery1 $ fmap ($ auth) $(selectQuery selectUpload "$!WHERE upload.token = ${tok} AND expires > CURRENT_TIMESTAMP AND upload.account = ${view auth :: Id Party}")

createToken :: (DBM m, EntropyM c m) => (Id Token -> DBTransaction a) -> m a
createToken insert = do
  gen <- fmap (Id . Base64.encode) <$> entropyBytesGenerator 24
  let loop = do
        tok <- liftIO gen
        r <- dbQuery1 [pgSQL|SELECT token FROM token WHERE token = ${tok}|]
        case r `asTypeOf` Just tok of
          Nothing -> insert tok
          Just _ -> loop
  dbTransaction $ do
    dbQuery "LOCK TABLE token IN SHARE ROW EXCLUSIVE MODE"
    loop

createLoginToken :: (DBM m, EntropyM c m) => SiteAuth -> Bool -> m LoginToken
createLoginToken auth passwd = do
  when passwd $ void $ dbExecute [pgSQL|DELETE FROM login_token WHERE account = ${view auth :: Id Party} AND password|]
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO login_token (token, account, password) VALUES (${tok}, ${view auth :: Id Party}, ${passwd}) RETURNING token, expires|]
  return $ LoginToken
    { loginAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , loginPasswordToken = passwd
    }

sessionDuration :: Bool -> Offset
sessionDuration False = 7*24*60*60
sessionDuration True = 30*60

createSession :: (DBM m, EntropyM c m) => SiteAuth -> Bool -> m Session
createSession auth su = do
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO session (token, expires, account, superuser) VALUES (${tok}, CURRENT_TIMESTAMP + ${sessionDuration su}::interval, ${view auth :: Id Party}, ${su}) RETURNING token, expires|]
  return $ Session
    { sessionAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , sessionSuperuser = su
    }

createUpload :: (DBM m, EntropyM c m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Int64 -> m Upload
createUpload vol name size = do
  auth <- peek
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO upload (token, account, volume, filename, size) VALUES (${tok}, ${view auth :: Id Party}, ${volumeId vol}, ${name}, ${size}) RETURNING token, expires|]
  return $ Upload
    { uploadAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , uploadFilename = name
    , uploadSize = size
    }

removeLoginToken :: DBM m => LoginToken -> m Bool
removeLoginToken tok =
  dbExecute1 [pgSQL|DELETE FROM login_token WHERE token = ${view tok :: Id Token}|]

removeSession :: (DBM m) => Session -> m Bool
removeSession tok =
  dbExecute1 [pgSQL|DELETE FROM session WHERE token = ${view tok :: Id Token}|]

removeUpload :: (DBM m, MonadStorage c m) => Upload -> m Bool
removeUpload tok = do
  r <- dbExecute1 [pgSQL|DELETE FROM upload WHERE token = ${view tok :: Id Token}|]
  when r $ liftIO . removeLink =<< peeks (uploadFile tok)
  return r
