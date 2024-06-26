module Data.Letter exposing (Letter(..), isCorrect, isGreaterThan, toChar)


type Letter
    = Correct Char
    | Possible Char
    | Impossible Char


isCorrect : Letter -> Bool
isCorrect letter =
    case letter of
        Correct _ ->
            True

        _ ->
            False


isGreaterThan : Letter -> Letter -> Bool
isGreaterThan second first =
    case ( first, second ) of
        ( Correct _, Possible _ ) ->
            True

        ( Correct _, Impossible _ ) ->
            True

        ( Possible _, Impossible _ ) ->
            True

        _ ->
            False


toChar : Letter -> Char
toChar letter =
    case letter of
        Correct ch ->
            ch

        Possible ch ->
            ch

        Impossible ch ->
            ch
