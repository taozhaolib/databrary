module Databrary.Model.Token
  ( module Databrary.Model.Types.Token
  ) where

import qualified Data.ByteString.Base64 as Base64

generateToken :: AppHandler TokenId
generateToken = Base64.encode <$> entropyBytes 24

getSession :: TokenId -> AppHandler (Maybe SessionToken)
getSession tok = pgQuery1 $(selectQuery sessionTokenSelector "WHERE session.token = ${tok}")
