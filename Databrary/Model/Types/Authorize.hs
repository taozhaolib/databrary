module Databrary.Model.Types.Authorize
  ( Access(..)
  , Authorization(..)
  , Authorize(..)
  ) where

import Databrary.Model.Permission
import Databrary.Model.Types.Party
import Data.Time (UTCTime)

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
