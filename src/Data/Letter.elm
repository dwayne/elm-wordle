module Data.Letter exposing (Letter(..), isCorrect, toChar)


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


toChar : Letter -> Char
toChar letter =
    case letter of
        Correct ch ->
            ch

        Possible ch ->
            ch

        Impossible ch ->
            ch
