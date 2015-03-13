module Databrary.Controller.Container where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Action

postContainer :: API -> Id Container -> AppRAction
createContainer :: API -> Id Volume -> AppRAction
