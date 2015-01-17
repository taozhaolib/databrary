module Databrary.Model.Authorize
  ( module Databrary.Model.Types.Authorize
  ) where

import Databrary.Model.Types.Authorize

nobodyAuthorization :: Authorization
nobodyAuthorization = Authorization
  { authorizeChild = nobodyParty
  , authorizeParent = rootParty
  , authorizeAccess = mempty
  }
