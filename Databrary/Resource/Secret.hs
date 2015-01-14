{-# LANGUAGE FlexibleInstances #-}
module Databrary.Resource.Secret
  ( Secret
  , initSecret
  ) where
 
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.ByteString as BS
import Data.Configurator (Config, require)

newtype Secret = Secret BS.ByteString

initSecret :: Config -> IO Secret
initSecret conf = Secret <$>
  require conf "secret"
