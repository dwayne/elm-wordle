module Data.History exposing (History, Past(..), empty, getPast, update)

import Data.Guess exposing (Guess)
import Data.Letter as Letter exposing (Letter)
import Dict exposing (Dict)


type History
    = History (Dict Char Past)


type Past
    = Correct
    | Possible
    | Impossible


empty : History
empty =
    History Dict.empty


update : Guess -> History -> History
update guess (History pasts) =
    History <| List.foldl updatePast pasts guess


updatePast : Letter -> Dict Char Past -> Dict Char Past
updatePast letter =
    Dict.update
        (Letter.toChar letter)
        (\maybePast ->
            case maybePast of
                Just past ->
                    if past == Possible && Letter.isCorrect letter then
                        Just Correct

                    else
                        maybePast

                Nothing ->
                    Just <| letterToPast letter
        )


letterToPast : Letter -> Past
letterToPast letter =
    case letter of
        Letter.Correct _ ->
            Correct

        Letter.Possible _ ->
            Possible

        Letter.Impossible _ ->
            Impossible


getPast : Char -> History -> Maybe Past
getPast ch (History pasts) =
    Dict.get ch pasts
