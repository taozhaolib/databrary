module ClassificationSelect (directive) where

import Data.Array (findIndex)

import Angular.Scope (modifyScope)

import Util
import Constants (Constants(..))

link const scope = do
  modifyScope (\s -> return {
    classifications: Data.Array.Unsafe.tail const.classification
  , selects: mapRange (show <<< ((>) s.value)) (Data.Array.length const.classification)
  , update: modifyScope (\sc -> return {
      value: findIndex ((==) "false") sc.selects
    }) scope
  }) scope

directive (Constants const) = do
  returnEff {
    restrict: "E"
  , templateUrl: "asset/classificationSelect.html"
  , scope: {
      value: "=ngModel"
    , name: "@"
    }
  , link: unsafeEff <<< link const
  }
