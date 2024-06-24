module Data.Word exposing (Word, fromString, toLetters, toString, zip)

import Data.Bag as Bag exposing (Bag)
import Data.Dictionary as Dictionary exposing (Dictionary)


type Word
    = Word String


fromString : Dictionary -> String -> Maybe Word
fromString d s =
    if Dictionary.contains s d then
        Just <| Word s

    else
        Nothing


zip : Word -> Word -> List ( Char, Char )
zip (Word w1) (Word w2) =
    --
    -- Assumes |w1| == |w2|.
    --
    zipHelper [] w1 w2


zipHelper : List ( Char, Char ) -> String -> String -> List ( Char, Char )
zipHelper pairs s1 s2 =
    case ( String.uncons s1, String.uncons s2 ) of
        ( Just ( ch1, rest1 ), Just ( ch2, rest2 ) ) ->
            zipHelper ( ( ch1, ch2 ) :: pairs ) rest1 rest2

        ( Nothing, Nothing ) ->
            List.reverse pairs

        _ ->
            []


toLetters : Word -> Bag Char
toLetters (Word w) =
    toLettersHelper Bag.empty w


toLettersHelper : Bag Char -> String -> Bag Char
toLettersHelper bag s =
    case String.uncons s of
        Just ( ch, rest ) ->
            toLettersHelper (Bag.insert ch bag) rest

        Nothing ->
            bag


toString : Word -> String
toString (Word w) =
    w
