module Lib.Random.Set exposing (sample)

import Lib.Random as Random
import Random
import Set exposing (Set)


sample : Set a -> Random.Generator (Maybe a)
sample =
    Set.toList >> Random.sample
