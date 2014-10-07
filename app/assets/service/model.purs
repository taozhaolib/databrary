module Model where

import Angular.DI (Dependency)
import Segment (Segments())

foreign import data Models :: *

instance modelService :: Dependency Models where
  name = "modelService"

foreign import getSegments 
  "function getSegments(Models) {\
  \ return Models.Segment;\
  \}" :: Models -> Segments
