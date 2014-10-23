module ClassificationSelect (Locals(), directive) where

import Control.Monad.Eff (returnE)
import Data.Array (findIndex)
import Angular.Scope (Scope(), ReadWriteEff(), readScope)
import Util
import Constants

type Locals =
  ( value :: Number
  , name :: String
  , classification :: [String]
  , max :: Number
  , check :: [Boolean]
  , update :: forall e . ReadWriteEff e Unit
  )

-- link :: forall e . Scope Locals -> ReadWriteEff e Unit
link scope = updateScope init scope where 
  update :: Object Locals -> Object Locals
  update s = s { value = findIndex id s.check }
  init :: Object Locals -> Object Locals
  init s = s
    { classification = constants.classification
    , max = Constants.classification "PUBLIC"
    , check = mapRange ((<=) s.value) (Data.Array.length constants.classification)
    , update = updateScope update scope
    }

directive = do
  returnE {
    restrict: "E"
  , templateUrl: "asset/classificationSelect.html"
  , scope: {
      value: "=ngModel"
    , name: "@"
    }
  , link: unsafeEff <<< link
  }
