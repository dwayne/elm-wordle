module Data.Wordle exposing
    ( Config
    , State
    , Status(..)
    , Wordle
    , maxAttemptsAllowed
    , minAttemptsAllowed
    , start
    , toConfigAndState
    , try
    )

import Data.Attempt as Attempt exposing (Attempt)
import Data.Bag as Bag exposing (Bag)
import Data.History as History exposing (History)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word exposing (Word)


type Wordle
    = Wordle Config State


type alias Config =
    { numAttemptsAllowed : Int
    , target : Word
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


start : Int -> Word -> Wordle
start numAttemptsAllowed target =
    let
        config =
            { numAttemptsAllowed = clamp minAttemptsAllowed maxAttemptsAllowed numAttemptsAllowed
            , target = target
            , chars = Word.toChars target
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
                    Attempt.try (Just config.chars) config.target guess

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


toConfigAndState : Wordle -> { config : Config, state : State }
toConfigAndState (Wordle config state) =
    { config = config, state = state }
