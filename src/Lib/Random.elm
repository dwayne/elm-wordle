module Lib.Random exposing (sample)

import Lib.List as List
import Random


sample : List a -> Random.Generator (Maybe a)
sample xs =
    Random.map (\i -> List.getAt i xs) (Random.int 0 (List.length xs - 1))
