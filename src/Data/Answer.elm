module Data.Answer exposing (Answer, fromString, toChars, toString, zip)

import Data.Bag as Bag exposing (Bag)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Word as Word exposing (Word)


type Answer
    = Answer String


fromString : Dictionary -> String -> Maybe Answer
fromString d s =
    if Dictionary.containsAnswer s d then
        Just <| Answer s

    else
        Nothing


zip : Answer -> Word -> List ( Char, Char )
zip (Answer answer) word =
    zipHelper [] answer (Word.toString word)


zipHelper : List ( Char, Char ) -> String -> String -> List ( Char, Char )
zipHelper pairs s1 s2 =
    case ( String.uncons s1, String.uncons s2 ) of
        ( Just ( ch1, rest1 ), Just ( ch2, rest2 ) ) ->
            zipHelper (( ch1, ch2 ) :: pairs) rest1 rest2

        _ ->
            List.reverse pairs


toChars : Answer -> Bag Char
toChars (Answer answer) =
    String.foldl Bag.insert Bag.empty answer


toString : Answer -> String
toString (Answer answer) =
    answer
