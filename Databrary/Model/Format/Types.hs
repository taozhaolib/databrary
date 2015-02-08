{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
module Databrary.Model.Format.Types
  ( Format(..)
  , MonadHasFormat
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int16)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Format = Int16

data Format = Format
  { formatId :: Id Format
  , formatMimeType :: BS.ByteString
  , formatExtension :: [String]
  , formatName :: T.Text
  }

instance Kinded Format where
  kindOf _ = "format"

makeHasRec ''Format ['formatId]
deriveLift ''Format
