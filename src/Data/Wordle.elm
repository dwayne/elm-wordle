module Data.Wordle exposing
    ( Config
    , Details
    , State
    , Status(..)
    , Wordle
    , guess
    , inspect
    , maxGuessesAllowed
    , minGuessesAllowed
    , start
    )

import Data.Answer as Answer exposing (Answer)
import Data.Bag as Bag exposing (Bag)
import Data.Guess as Guess exposing (Guess)
import Data.History as History exposing (History)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word exposing (Word)


type Wordle
    = Wordle Config State


type alias Config =
    { numGuessesAllowed : Int
    , answer : Answer
    , chars : Bag Char
    }


minGuessesAllowed : Int
minGuessesAllowed =
    1


maxGuessesAllowed : Int
maxGuessesAllowed =
    10


type alias State =
    { pastGuesses : List Guess
    , history : History
    , status : Status
    }


type Status
    = InProgress
    | Won
    | Lost


start : Int -> Answer -> Wordle
start numGuessesAllowed answer =
    let
        config =
            { numGuessesAllowed = clamp minGuessesAllowed maxGuessesAllowed numGuessesAllowed
            , answer = answer
            , chars = Answer.toChars answer
            }

        state =
            { pastGuesses = []
            , history = History.empty
            , status = InProgress
            }
    in
    Wordle config state


guess : Word -> Wordle -> Wordle
guess word (Wordle config state) =
    Wordle config <|
        if state.status == InProgress then
            let
                attempt =
                    Guess.guess (Just config.chars) config.answer word

                pastGuesses =
                    attempt :: state.pastGuesses

                history =
                    List.foldl History.update state.history attempt

                status =
                    if Guess.isCorrect attempt then
                        Won

                    else if List.length pastGuesses == config.numGuessesAllowed then
                        Lost

                    else
                        InProgress
            in
            { state | pastGuesses = pastGuesses, history = history, status = status }

        else
            state


type alias Details =
    { numGuessesAllowed : Int
    , answer : Answer
    , pastGuesses : List Guess
    , history : History
    , status : Status
    }


inspect : Wordle -> Details
inspect (Wordle { numGuessesAllowed, answer } { pastGuesses, history, status }) =
    { numGuessesAllowed = numGuessesAllowed
    , answer = answer
    , pastGuesses = List.reverse pastGuesses
    , history = history
    , status = status
    }
