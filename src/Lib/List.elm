module Lib.List exposing (getAt)


getAt : Int -> List a -> Maybe a
getAt i xs =
    if i < 0 then
        Nothing

    else
        List.head <| List.drop i xs
