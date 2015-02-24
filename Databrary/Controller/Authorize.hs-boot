module Databrary.Controller.Authorize where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action
import Databrary.Controller.Party

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

postAuthorize :: API -> PartyTarget -> AuthorizeTarget -> AppRAction
