{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Container.SQL
  ( selectContainer
  ) where

import Databrary.Model.SQL
import Databrary.Model.Container.Types

containerRow :: Selector -- ^ @Maybe 'Consent' -> 'Permission' -> 'Container'@
containerRow = selectColumns 'Container "container" ["id", "top", "name", "date"]

selectContainer :: Selector -- ^ @'Volume' -> 'Container'@
selectContainer = selectJoin '($)
  [ containerRow
  , maybeJoinOn "container.id = slot_consent.container AND slot_consent.segment = '(,)'"
    $ selector "slot_consent" "consent"
  ]
