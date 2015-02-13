{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , identitySuperuser
  , MonadHasIdentity
  ) where

import Control.Applicative (Applicative)
import Control.Lens (set)
import Control.Monad.Reader (MonadReader)
import Data.Monoid (mempty)

import Control.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Token.Types

data Identity
  = UnIdentified
  | Identified { identitySession :: SessionToken }

-- makeLensesFor [("identitySession", "identitySession'")] ''Identity

instance Has Party Identity where
  view f UnIdentified = fmap (const UnIdentified) $ f nobodyParty
  view f (Identified t) = fmap (\p -> Identified (set view p t)) $ f $ see t
  see UnIdentified = nobodyParty
  see (Identified t) = see t

instance Has (Id Party) Identity where
  view f UnIdentified = fmap (const UnIdentified) $ f $ partyId nobodyParty
  view f (Identified t) = fmap (\p -> Identified (set view p t)) $ f $ see t
  see UnIdentified = partyId nobodyParty
  see (Identified t) = see t

instance Has Access Identity where
  view f UnIdentified = fmap (const UnIdentified) $ f mempty
  view f (Identified t) = fmap (\p -> Identified (set view p t)) $ f $ see t
  see UnIdentified = mempty
  see (Identified t) = see t

identitySuperuser :: Identity -> Bool
identitySuperuser UnIdentified = False
identitySuperuser (Identified t) = sessionSuperuser t

type MonadHasIdentity c m = (Functor m, Applicative m, MonadReader c m, Has Identity c, Has Party c, Has (Id Party) c)
