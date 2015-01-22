module Databrary.Model.Types.Authorize
  ( Access(..)
  , accessSite, accessMember
  , accessPermission
  , Authorization(..)
  , Authorize(..)
  ) where

import Data.Monoid (Monoid(..))
import Data.Time (UTCTime)

import Control.Monad.Has (Has(..))
import Databrary.Model.Types.Permission
import Databrary.Model.Types.Party

data Access = Access
  { _accessSite :: !Permission
  , _accessMember :: !Permission
  }

_accessPermission :: Access -> Permission
_accessPermission (Access s m) = min s m

accessSite, accessMember, accessPermission :: Has Access a => a -> Permission
accessSite = _accessSite . had
accessMember = _accessMember . had
accessPermission = _accessPermission . had

instance Monoid Access where
  mempty = Access PermissionNONE PermissionNONE
  mappend (Access s1 m1) (Access s2 m2) = Access (max s1 s2) (max m1 m2)

-- consider: makeClassy ''Access

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

instance Has Access Authorization where
  had = had . authorizeAccess

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }

instance Has Access Authorize where
  had = had . authorization
