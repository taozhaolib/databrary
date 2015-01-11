module Databrary.Model.Types.Authorize
  ( Access(..)
  , Authorization(..)
  ) where

import Databary.Model.Permission
import Databary.Model.Types.Party

data Access = Access
  { accessSite :: !Permission
  , accessMember :: !Permission
  }

-- consider: makeClassy ''Access

data Authorization = Authorization
  { authorizeChild :: Party
  , authorizeParty :: Party
  , authorizeAccess :: !Access
  }

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }
