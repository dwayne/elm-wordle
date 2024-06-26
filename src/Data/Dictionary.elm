module Data.Dictionary exposing (Dictionary, contains, fromList)

import Set exposing (Set)


type Dictionary
    = Dictionary
        --
        -- N.B. The set of answers and the set of nonAnswers are disjoint.
        --
        { answers : Set String
        , nonAnswers : Set String
        }


fromList :
    { wordLength : Int
    , answers : List String
    , nonAnswers : List String
    }
    -> Dictionary
fromList { wordLength, answers, nonAnswers } =
    Dictionary <|
        if wordLength >= 1 && wordLength <= 10 then
            let
                answersSet =
                    processWords wordLength answers

                nonAnswersSet =
                    Set.diff
                        (processWords wordLength nonAnswers)
                        answersSet
            in
            { answers = answersSet
            , nonAnswers = nonAnswersSet
            }

        else
            { answers = Set.empty
            , nonAnswers = Set.empty
            }


processWords : Int -> List String -> Set String
processWords n =
    List.filterMap
        (\s ->
            let
                t =
                    String.trim s
            in
            if String.length (String.left n t) == n then
                Just <| String.toLower t

            else
                Nothing
        )
        >> Set.fromList


contains : String -> Dictionary -> Bool
contains s (Dictionary { answers, nonAnswers }) =
    Set.member s answers || Set.member s nonAnswers
