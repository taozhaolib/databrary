module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , nobodyAuthorization
  ) where

import Data.Monoid (mempty)

import Databrary.Model.Authorize.Types
import Databrary.Model.Party

nobodyAuthorization :: Authorization
nobodyAuthorization = Authorization
  { authorizeChild = nobodyParty
  , authorizeParent = rootParty
  , authorizeAccess = mempty
  }
