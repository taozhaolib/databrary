{-# LANGUAGE CPP #-}
module Databrary.Web.Types
  ( WebFileInfo(..)
  , WebFileMap
  , Web(..)
  , WebGeneratorM
  , WebGenerator
  , webGeneratorFail
  ) where

#ifdef DEVEL
import Control.Concurrent.MVar (MVar)
#endif
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT)
import Crypto.Hash (Digest, MD5)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import Databrary.Model.Time
import Databrary.Web (WebFilePath, webFileRel)

data WebFileInfo = WebFileInfo
  { webFileFormat :: BS.ByteString
  , webFileHash :: !(Digest MD5)
  , webFileTimestamp :: !Timestamp
  }

type WebFileMap = HM.HashMap WebFilePath WebFileInfo

data Web = Web
  { webCache ::
#ifdef DEVEL
    MVar
#endif
      WebFileMap
  }

type WebGeneratorM a = StateT WebFileMap (ExceptT String IO) a
type WebGenerator = (WebFilePath, Maybe WebFileInfo) -> WebGeneratorM Bool

webGeneratorFail :: WebGenerator
webGeneratorFail = lift . throwE . webFileRel . fst
