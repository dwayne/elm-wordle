module Data.Bag exposing (Bag, contains, empty, insert, remove)

import Dict exposing (Dict)


type Bag a
    = Bag (Dict a Int)


empty : Bag a
empty =
    Bag Dict.empty


contains : comparable -> Bag comparable -> Bool
contains element (Bag contents) =
    Dict.member element contents


insert : comparable -> Bag comparable -> Bag comparable
insert element (Bag contents) =
    Bag <|
        Dict.update
            element
            (\maybeCount ->
                case maybeCount of
                    Just count ->
                        Just <| count + 1

                    Nothing ->
                        Just 1
            )
            contents


remove : comparable -> Bag comparable -> Bag comparable
remove element (Bag contents) =
    Bag <|
        Dict.update
            element
            (\maybeCount ->
                case maybeCount of
                    Just count ->
                        if count == 1 then
                            Nothing

                        else
                            Just <| count - 1

                    Nothing ->
                        Nothing
            )
            contents
