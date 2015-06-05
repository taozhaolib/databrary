{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , MonadHasIdentity
  , foldIdentity
  , identityVerf
  , identitySuperuser
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as BS

import Databrary.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Token.Types

data Identity
  = UnIdentified
  | Identified Session
  | ReIdentified SiteAuth

instance Has SiteAuth Identity where
  view UnIdentified = nobodySiteAuth
  view (Identified Session{ sessionAccountToken = AccountToken{ tokenAccount = t } }) = t
  view (ReIdentified a) = a

instance Has Party Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Account Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has (Id Party) Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Access Identity where
  view = view . (view :: Identity -> SiteAuth)

type MonadHasIdentity c m = (Functor m, Applicative m, MonadReader c m, Has Identity c, Has SiteAuth c, Has Party c, Has (Id Party) c, Has Access c)

foldIdentity :: a -> (Session -> a) -> Identity -> a
foldIdentity _ i (Identified s) = i s
foldIdentity u _ _ = u

identityVerf :: Identity -> Maybe BS.ByteString
identityVerf = foldIdentity Nothing (Just . sessionVerf)

identitySuperuser :: Identity -> Bool
identitySuperuser UnIdentified = False
identitySuperuser (Identified t) = sessionSuperuser t
identitySuperuser (ReIdentified _) = True

