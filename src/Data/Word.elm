module Data.Word exposing (Word, fromString, toString)

import Data.Dictionary as Dictionary exposing (Dictionary)


type Word
    = Word String


fromString : Dictionary -> String -> Maybe Word
fromString d s =
    if Dictionary.contains s d then
        Just <| Word s

    else
        Nothing


toString : Word -> String
toString (Word w) =
    w
