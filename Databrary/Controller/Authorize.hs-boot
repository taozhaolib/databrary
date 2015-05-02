module Databrary.Controller.Authorize where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action
import Databrary.Controller.Party

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

viewAuthorize :: AppRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize :: AppRoute (API, PartyTarget, AuthorizeTarget)
