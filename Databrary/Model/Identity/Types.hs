{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , identitySuperuser
  , MonadHasIdentity
  ) where

import Control.Applicative (Applicative)
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
  view UnIdentified = nobodyParty
  view (Identified t) = view t

instance Has (Id Party) Identity where
  view UnIdentified = partyId nobodyParty
  view (Identified t) = view t

instance Has Access Identity where
  view UnIdentified = mempty
  view (Identified t) = view t

identitySuperuser :: Identity -> Bool
identitySuperuser UnIdentified = False
identitySuperuser (Identified t) = sessionSuperuser t

type MonadHasIdentity c m = (Functor m, Applicative m, MonadReader c m, Has Identity c, Has Party c, Has (Id Party) c)
