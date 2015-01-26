{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Types.Authorize
  ( Access(..)
  , accessSite, accessMember
  , accessPermission
  , Authorization(..)
  , Authorize(..)
  , AuthParty(..)
  , PartyAuth(..)
  ) where

import Control.Lens (Lens', makeLensesFor)
import Data.Monoid (Monoid(..))
import Data.Time (UTCTime)

import Control.Has (Has(..), makeHasFor)
import Databrary.Model.Types.Permission
import Databrary.Model.Types.Party

data Access = Access
  { _accessSite :: !Permission
  , _accessMember :: !Permission
  }

makeLensesFor [("_accessSite", "accessSite'"), ("_accessMember", "accessMember'")] ''Access

accessSite, accessMember :: Has Access a => Lens' a Permission
accessSite = view . accessSite'
accessMember = view . accessMember'

_accessPermission :: Access -> Permission
_accessPermission (Access s m) = min s m

accessPermission :: Has Access a => a -> Permission
accessPermission = _accessPermission . see

instance Monoid Access where
  mempty = Access PermissionNONE PermissionNONE
  mappend (Access s1 m1) (Access s2 m2) = Access (max s1 s2) (max m1 m2)

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

makeHasFor ''Authorization
  [ ('authorizeAccess, [])
  ]

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }

makeHasFor ''Authorize
  [ ('authorization, [''Access])
  ]

-- |'Authorization' representing (access to) the parent
newtype AuthParty = AuthParty { authPartyAuthorization :: Authorization }

makeHasFor ''AuthParty
  [ ('authPartyAuthorization, [''Access])
  ]

instance Has Party AuthParty where
  view f (AuthParty (Authorization a c p)) =
    fmap (\p' -> AuthParty (Authorization a c p')) (f p)
  see (AuthParty a) = authorizeParent a

-- |'Authorization' representing the child('s access)
newtype PartyAuth = PartyAuth { partyAuthAuthorization :: Authorization }

makeHasFor ''PartyAuth
  [ ('partyAuthAuthorization, [''Access])
  ]

instance Has Party PartyAuth where
  view f (PartyAuth (Authorization a c p)) =
    fmap (\c' -> PartyAuth (Authorization a c' p)) (f c)
  see (PartyAuth a) = authorizeChild a

