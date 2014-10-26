module Constants (
    Constants(..)
  , constants
  , Permission(..)
  , permission
  , Classification(..)
  , classification
  , Consent(..)
  , consent
  ) where

import Data.Array (elemIndex)

newtype Permission = Permission Number
newtype Classification = Classification Number
newtype Consent = Consent Number

type Constants = {
    permission :: [String]
  , classification :: [String]
  , consent :: [String]
  }

foreign import constants :: Constants

-- it would be nice to auto-generate these:

permission =
  { none:   p "NONE"
  , public: p "PUBLIC"
  , shared: p "SHARED"
  , read:   p "READ"
  , edit:   p "EDIT"
  , admin:  p "ADMIN"
  } where
  p n = Permission $ elemIndex n constants.permission

classification =
  { private:    c "PRIVATE"
  , restricted: c "RESTRICTED"
  , shared:     c "SHARED"
  , public:     c "PUBLIC"
  } where
  c n = Classification $ elemIndex n constants.classification

consent =
  { none:     c "NONE"
  , private:  c "PRIVATE"
  , shared:   c "SHARED"
  , excerpts: c "EXCERPTS"
  , public:   c "PUBLIC"
  } where
  c n = Consent $ elemIndex n constants.consent
