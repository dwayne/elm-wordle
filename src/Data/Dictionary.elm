module Data.Dictionary exposing (Dictionary, fromList, contains)

import Set exposing (Set)


type Dictionary
    = Dictionary (Set String)


fromList : List String -> Dictionary
fromList =
    List.filterMap
        (\s ->
            let
                t =
                    String.trim s
            in
            if String.left 5 t == t then
                Just <| String.toLower t

            else
                Nothing
        )
    >> Set.fromList
    >> Dictionary


contains : String -> Dictionary -> Bool
contains s (Dictionary words) =
    Set.member s words
