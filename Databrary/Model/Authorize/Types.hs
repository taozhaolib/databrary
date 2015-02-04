{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Authorize.Types
  ( Access(..)
  , accessSite, accessMember
  , Authorization(..)
  , Authorize(..)
  , AuthParty(..)
  , PartyAuth(..)
  ) where

import Control.Monad (join)
import Control.Lens (Lens', makeLensesFor)
import Data.Monoid (Monoid(..))
import Data.Time (UTCTime)

import Control.Has (Has(..), makeHasFor)
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types

data Access = Access
  { _accessSite :: !Permission
  , _accessMember :: !Permission
  }

makeLensesFor [("_accessSite", "accessSite'"), ("_accessMember", "accessMember'")] ''Access

accessSite, accessMember :: Has Access a => Lens' a Permission
accessSite = view . accessSite'
accessMember = view . accessMember'

instance Has Permission Access where
  view f = fmap (join Access) . f . see
  see (Access s m) = min s m

instance Bounded Access where
  minBound = Access minBound minBound
  maxBound = Access maxBound maxBound

instance Monoid Access where
  mempty = Access PermissionNONE PermissionNONE
  mappend (Access s1 m1) (Access s2 m2) = Access (max s1 s2) (max m1 m2)

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

makeHasFor ''Authorization
  [ ('authorizeAccess, [''Permission])
  ]

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }

makeHasFor ''Authorize
  [ ('authorization, [''Access, ''Permission])
  ]

-- |'Authorization' representing (access to) the parent
newtype AuthParty = AuthParty { authPartyAuthorization :: Authorization }

makeHasFor ''AuthParty
  [ ('authPartyAuthorization, [''Access, ''Permission])
  ]

instance Has Party AuthParty where
  view f (AuthParty (Authorization a c p)) =
    fmap (\p' -> AuthParty (Authorization a c p')) (f p)
  see (AuthParty a) = authorizeParent a

-- |'Authorization' representing the child('s access)
newtype PartyAuth = PartyAuth { partyAuthAuthorization :: Authorization }

makeHasFor ''PartyAuth
  [ ('partyAuthAuthorization, [''Access, ''Permission])
  ]

instance Has Party PartyAuth where
  view f (PartyAuth (Authorization a c p)) =
    fmap (\c' -> PartyAuth (Authorization a c' p)) (f c)
  see (PartyAuth a) = authorizeChild a

