module ClassificationSelect (directive) where

import Data.Array (findIndex)
import Angular.Scope (modifyScope)
import Util
import Constants

link scope = do
  modifyScope (\s -> return {
    classifications: constants.classification
  , max: Constants.unsafePermission "PUBLIC"
  , check: mapRange ((<=) s.value) (Data.Array.length constants.classification)
  , update: modifyScope (\sc -> return {
      value: findIndex id sc.selects
    }) scope
  }) scope

directive = do
  returnEff {
    restrict: "E"
  , templateUrl: "asset/classificationSelect.html"
  , scope: {
      value: "=ngModel"
    , name: "@"
    }
  , link: unsafeEff <<< link
  }
