module Data.History exposing (History, Status(..), empty, getStatus, update)

import Dict exposing (Dict)


type History
    = History (Dict Char Status)


type Status
    = Correct
    | AlmostCorrect
    | Incorrect


empty : History
empty =
    History Dict.empty


update : Char -> Status -> History -> History
update ch newStatus (History dict) =
    History <|
        Dict.update
            ch
            (\maybeOldStatus ->
                case maybeOldStatus of
                    Just oldStatus ->
                        if oldStatus == AlmostCorrect then
                            if newStatus == Correct then
                                Just newStatus

                            else
                                maybeOldStatus

                        else
                            maybeOldStatus

                    Nothing ->
                        Just newStatus
            )
            dict


getStatus : Char -> History -> Maybe Status
getStatus ch (History dict) =
    Dict.get ch dict
