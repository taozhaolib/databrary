{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
module Databrary.Model.Format.Types
  ( Format(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int16)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Control.Has (Has(..))
import Databrary.Kind
import Databrary.Model.Id.Types

type instance IdType Format = Int16

data Format = Format
  { formatId :: Id Format
  , formatMimeType :: BS.ByteString
  , formatExtension :: [String]
  , formatName :: T.Text
  }

instance Has (Id Format) Format where
  view f a = fmap (\i -> a{ formatId = i }) $ f $ formatId a
  see = formatId
instance Kinded Format where
  kindOf _ = "format"

deriveLift ''Format
