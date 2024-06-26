module Data.Letter exposing (Letter(..), isCorrect, isGreaterThan, toChar)


type
    Letter
    --
    -- TODO: Use Correct, Possible, and NotPossible/Impossible.
    --
    = Correct Char
    | AlmostCorrect Char
    | Incorrect Char


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
        ( Correct _, AlmostCorrect _ ) ->
            True

        ( Correct _, Incorrect _ ) ->
            True

        ( AlmostCorrect _, Incorrect _ ) ->
            True

        _ ->
            False


toChar : Letter -> Char
toChar letter =
    case letter of
        Correct ch ->
            ch

        AlmostCorrect ch ->
            ch

        Incorrect ch ->
            ch
