module Data.Dictionary exposing (Dictionary, contains, fromList)

import Set exposing (Set)


type Dictionary
    = Dictionary (Set String)


fromList : Int -> List String -> Dictionary
fromList n =
    --
    -- n represents the length of each word in the dictionary.
    --
    if n >= 1 && n <= 10 then
        List.filterMap
            (\s ->
                let
                    t =
                        String.trim s
                in
                if String.length (String.left n t) == n then
                    Just <| String.toLower t

                else
                    Nothing
            )
            >> Set.fromList
            >> Dictionary

    else
        always <| Dictionary Set.empty


contains : String -> Dictionary -> Bool
contains s (Dictionary words) =
    Set.member s words
