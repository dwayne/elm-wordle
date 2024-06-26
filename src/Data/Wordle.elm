module Data.Wordle exposing
    ( Config
    , Details
    , State
    , Status(..)
    , Wordle
    , inspect
    , maxAttemptsAllowed
    , minAttemptsAllowed
    , start
    , try
    )

import Data.Answer as Answer exposing (Answer)
import Data.Attempt as Attempt exposing (Attempt)
import Data.Bag as Bag exposing (Bag)
import Data.History as History exposing (History)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word exposing (Word)


type Wordle
    = Wordle Config State


type alias Config =
    { numAttemptsAllowed : Int
    , answer : Answer
    , chars : Bag Char
    }


minAttemptsAllowed : Int
minAttemptsAllowed =
    1


maxAttemptsAllowed : Int
maxAttemptsAllowed =
    10


type alias State =
    { pastAttempts : List Attempt
    , history : History
    , status : Status
    }


type Status
    = InProgress
    | Won
    | Lost


start : Int -> Answer -> Wordle
start numAttemptsAllowed answer =
    let
        config =
            { numAttemptsAllowed = clamp minAttemptsAllowed maxAttemptsAllowed numAttemptsAllowed
            , answer = answer
            , chars = Answer.toChars answer
            }

        state =
            { pastAttempts = []
            , history = History.empty
            , status = InProgress
            }
    in
    Wordle config state


try : Word -> Wordle -> Wordle
try guess (Wordle config state) =
    Wordle config <|
        if state.status == InProgress then
            let
                attempt =
                    Attempt.try (Just config.chars) config.answer guess

                pastAttempts =
                    attempt :: state.pastAttempts

                history =
                    List.foldl History.update state.history attempt

                status =
                    if Attempt.isCorrect attempt then
                        Won

                    else if List.length pastAttempts == config.numAttemptsAllowed then
                        Lost

                    else
                        InProgress
            in
            { state | pastAttempts = pastAttempts, history = history, status = status }

        else
            state


type alias Details =
    { numAttemptsAllowed : Int
    , answer : Answer
    , pastAttempts : List Attempt
    , history : History
    , status : Status
    }


inspect : Wordle -> Details
inspect (Wordle { numAttemptsAllowed, answer } { pastAttempts, history, status }) =
    { numAttemptsAllowed = numAttemptsAllowed
    , answer = answer
    , pastAttempts = List.reverse pastAttempts
    , history = history
    , status = status
    }
