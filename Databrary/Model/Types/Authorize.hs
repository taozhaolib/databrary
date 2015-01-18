module Databrary.Model.Types.Authorize
  ( Access(..)
  , Authorization(..)
  , Authorize(..)
  ) where

import Data.Monoid (Monoid(..))
import Data.Time (UTCTime)

import Databrary.Model.Permission
import Databrary.Model.Types.Party

data Access = Access
  { accessSite :: !Permission
  , accessMember :: !Permission
  }

instance Monoid Access where
  mempty = Access PermissionNONE PermissionNONE
  mappend (Access s1 m1) (Access s2 m2) = Access (max s1 s2) (max m1 m2)

-- consider: makeClassy ''Access

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }
