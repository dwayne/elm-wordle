module Data.History exposing (History, empty, getLetter, update)

import Data.Letter as Letter exposing (Letter)
import Dict exposing (Dict)


type History
    = History (Dict Char Letter)


empty : History
empty =
    History Dict.empty


update : Letter -> History -> History
update newLetter (History letters) =
    History <|
        Dict.update
            (Letter.toChar newLetter)
            (\maybeOldLetter ->
                case maybeOldLetter of
                    Just oldLetter ->
                        if newLetter |> Letter.isGreaterThan oldLetter then
                            Just newLetter

                        else
                            maybeOldLetter

                    Nothing ->
                        Just newLetter
            )
            letters


getLetter : Char -> History -> Maybe Letter
getLetter ch (History letters) =
    Dict.get ch letters
