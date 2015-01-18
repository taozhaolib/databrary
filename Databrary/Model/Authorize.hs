module Databrary.Model.Authorize
  ( module Databrary.Model.Types.Authorize
  , nobodyAuthorization
  ) where

import Data.Monoid (mempty)

import Databrary.Model.Types.Authorize
import Databrary.Model.Party

nobodyAuthorization :: Authorization
nobodyAuthorization = Authorization
  { authorizeChild = nobodyParty
  , authorizeParent = rootParty
  , authorizeAccess = mempty
  }
