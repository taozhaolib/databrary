{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Token
  ( module Databrary.Model.Token.Types
  , lookupSession
  , createSession
  , removeSession
  , lookupUpload
  , createUpload
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)

import Control.Has (view, peek)
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Time.Types
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party
import Databrary.Entropy
import Databrary.DB
import Databrary.Model.Token.Types
import Databrary.Model.Token.SQL

useTPG

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

lookupSession :: DBM m => BS.ByteString -> m (Maybe SessionToken)
lookupSession tok =
  dbQuery1 $(selectQuery selectSessionToken "$!WHERE session.token = ${tok} AND expires > CURRENT_TIMESTAMP")

lookupUpload :: (DBM m, MonadHasIdentity c m) => BS.ByteString -> m (Maybe UploadToken)
lookupUpload tok = do
  auth <- peek
  dbQuery1 $ fmap ($ auth) $(selectQuery selectUploadToken "$!WHERE upload.token = ${tok} AND expires > CURRENT_TIMESTAMP AND upload.account = ${view auth :: Id Party}")

sessionDuration :: Bool -> Offset
sessionDuration False = 7*24*60*60
sessionDuration True = 30*60

createSession :: (DBM m, EntropyM c m) => SiteAuth -> Bool -> m SessionToken
createSession auth su = do
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO session (token, expires, account, superuser) VALUES (${tok}, CURRENT_TIMESTAMP + ${sessionDuration su}::interval, ${view auth :: Id Party}, ${su}) RETURNING token, expires|]
  return $ SessionToken
    { sessionAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , sessionSuperuser = su
    }

removeSession :: (DBM m) => SessionToken -> m Bool
removeSession tok = (0 <) <$>
  dbExecute [pgSQL|DELETE FROM session WHERE token = ${view tok :: Id Token}|]

createUpload :: (DBM m, EntropyM c m, MonadHasIdentity c m) => Volume -> T.Text -> m UploadToken
createUpload vol name = do
  auth <- peek
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO upload (token, account, volume, filename) VALUES (${tok}, ${view auth :: Id Party}, ${volumeId vol}, ${name}) RETURNING token, expires|]
  return $ UploadToken
    { uploadAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , uploadFilename = name
    }
