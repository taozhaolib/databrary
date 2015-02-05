{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Token
  ( module Databrary.Model.Token.Types
  , lookupSession
  , sessionAuthorization
  , createSession
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64.URL as Base64
import Data.Maybe (fromJust)
import Database.PostgreSQL.Typed (pgSQL)

import Control.Has (see)
import Databrary.Time
import Databrary.Model.SQL
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Entropy
import Databrary.DB
import Databrary.Model.Token.Types
import Databrary.Model.Token.SQL

useTPG

createToken :: (DBM m, EntropyM c m) => (TokenId -> DBTransaction a) -> m a
createToken insert = do
  gen <- fmap Base64.encode <$> entropyBytesGenerator 24
  let loop = do
        tok <- liftIO gen
        r <- dbQuery1 [pgSQL|SELECT token FROM token WHERE token = ${tok}|]
        case r `asTypeOf` Just tok of
          Nothing -> insert tok
          Just _ -> loop
  dbTransaction $ do
    dbQuery "LOCK TABLE token IN SHARE ROW EXCLUSIVE MODE"
    loop

sessionAuthorization :: SessionToken -> PartyAuth
sessionAuthorization tok = PartyAuth $ Authorization
  { authorizeChild = see tok
  , authorizeParent = rootParty
  , authorizeAccess = sessionAccess tok
  }

lookupSession :: DBM m => TokenId -> m (Maybe SessionToken)
lookupSession tok = dbQuery1 $(selectQuery sessionTokenSelector "$!WHERE session.token = ${tok} AND expires > CURRENT_TIMESTAMP")

sessionDuration :: Bool -> Offset
sessionDuration False = 7*24*60*60
sessionDuration True = 30*60

createSession :: (DBM m, EntropyM c m) => PartyAuth -> Bool -> m SessionToken
createSession auth su = do
  (tok, ex) <- createToken $ \tok -> do
    dbQuery1' [pgSQL|INSERT INTO session (token, expires, account, superuser) VALUES (${tok}, CURRENT_TIMESTAMP + ${sessionDuration su}::interval, ${partyId p}, ${su}) RETURNING token, expires|]
  return $ SessionToken
    { sessionAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = fromJust (partyAccount p)
      }
    , sessionSuperuser = su
    , sessionAccess = see auth
    }
  where p = see auth
