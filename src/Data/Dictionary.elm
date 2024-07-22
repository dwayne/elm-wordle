module Data.Dictionary exposing
    ( Dictionary
    , answerGenerator
    , contains
    , containsAnswer
    , new
    , toWordLength
    )

import Lib.Random.Set as Set
import Random
import Set exposing (Set)


type Dictionary
    = Dictionary
        { wordLength : Int

        --
        -- N.B. The set of answers and the set of nonAnswers are disjoint.
        --
        , answers : Set String
        , nonAnswers : Set String
        }


new :
    { wordLength : Int
    , answers : List String
    , nonAnswers : List String
    }
    -> Dictionary
new { wordLength, answers, nonAnswers } =
    Dictionary <|
        if wordLength >= minWordLength && wordLength <= maxWordLength then
            let
                answersSet =
                    processWords wordLength answers

                nonAnswersSet =
                    Set.diff
                        (processWords wordLength nonAnswers)
                        answersSet
            in
            { wordLength = wordLength
            , answers = answersSet
            , nonAnswers = nonAnswersSet
            }

        else
            { wordLength = 0
            , answers = Set.empty
            , nonAnswers = Set.empty
            }


minWordLength : Int
minWordLength =
    1


maxWordLength : Int
maxWordLength =
    10


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


answerGenerator : Dictionary -> Random.Generator (Maybe String)
answerGenerator (Dictionary { answers }) =
    Set.sample answers


containsAnswer : String -> Dictionary -> Bool
containsAnswer s (Dictionary { answers }) =
    Set.member s answers


contains : String -> Dictionary -> Bool
contains s (Dictionary { answers, nonAnswers }) =
    Set.member s answers || Set.member s nonAnswers


toWordLength : Dictionary -> Int
toWordLength (Dictionary { wordLength }) =
    wordLength
