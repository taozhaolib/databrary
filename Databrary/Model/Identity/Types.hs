{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , identitySuperuser
  , MonadHasIdentity
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader)

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

identitySuperuser :: Identity -> Bool
identitySuperuser UnIdentified = False
identitySuperuser (Identified t) = sessionSuperuser t
identitySuperuser (ReIdentified _) = True

type MonadHasIdentity c m = (Functor m, Applicative m, MonadReader c m, Has Identity c, Has SiteAuth c, Has Party c, Has (Id Party) c, Has Access c)
