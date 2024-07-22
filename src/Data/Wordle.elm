module Data.Wordle exposing
    ( Action(..)
    , Config
    , Details
    , Outcome(..)
    , Reason(..)
    , State
    , Status(..)
    , Wordle
    , guess
    , inspect
    , perform
    , start
    )

import Data.Answer as Answer exposing (Answer)
import Data.Bag as Bag exposing (Bag)
import Data.Dictionary as Dictionary exposing (Dictionary)
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


type alias State =
    { currentInput : List Char
    , pastGuesses : List Guess
    , history : History
    , status : Status
    }


type Status
    = InProgress
    | Won Int
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
            { currentInput = []
            , pastGuesses = []
            , history = History.empty
            , status = InProgress
            }
    in
    Wordle config state


minGuessesAllowed : Int
minGuessesAllowed =
    1


maxGuessesAllowed : Int
maxGuessesAllowed =
    10


type Action
    = Input Char
    | Enter
    | Delete


type Outcome
    = AppendChar Char
    | RemoveChar Char
    | GuessAllowed Word
    | NoChange Reason


type Reason
    = Empty
    | GameOver
    | MaxLength Int
    | NotAWord String
    | NotEnoughLetters
        { actual : Int
        , expected : Int
        }


perform : Dictionary -> Action -> Wordle -> ( Wordle, Outcome )
perform dictionary action (Wordle config state) =
    if state.status == InProgress then
        let
            wordLength =
                Dictionary.toWordLength dictionary

            currentInput =
                state.currentInput

            currentInputLength =
                List.length currentInput
        in
        case action of
            Input ch ->
                if currentInputLength == wordLength then
                    ( Wordle config state, NoChange <| MaxLength wordLength )

                else
                    ( Wordle config { state | currentInput = ch :: currentInput }
                    , AppendChar ch
                    )

            Enter ->
                if currentInputLength == wordLength then
                    let
                        currentInputAsString =
                            currentInput
                                |> List.reverse
                                |> String.fromList

                        maybeWord =
                            Word.fromString dictionary currentInputAsString
                    in
                    case maybeWord of
                        Just word ->
                            ( Wordle config { state | currentInput = [] }
                            , GuessAllowed word
                            )

                        Nothing ->
                            ( Wordle config state, NoChange <| NotAWord currentInputAsString )

                else
                    ( Wordle config state
                    , NoChange <|
                        NotEnoughLetters
                            { actual = currentInputLength
                            , expected = wordLength
                            }
                    )

            Delete ->
                case currentInput of
                    [] ->
                        ( Wordle config state, NoChange Empty )

                    ch :: restCurrentInput ->
                        ( Wordle config { state | currentInput = restCurrentInput }
                        , RemoveChar ch
                        )

    else
        ( Wordle config state, NoChange GameOver )


guess : Word -> Wordle -> Wordle
guess word (Wordle config state) =
    Wordle config <|
        if state.status == InProgress then
            let
                g =
                    Guess.guess (Just config.chars) config.answer word

                pastGuesses =
                    g :: state.pastGuesses

                numGuesses =
                    List.length pastGuesses

                history =
                    History.update g state.history

                status =
                    if Guess.isCorrect g then
                        Won numGuesses

                    else if numGuesses == config.numGuessesAllowed then
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
    , currentInput : List Char
    , pastGuesses : List Guess
    , history : History
    , status : Status
    }


inspect : Wordle -> Details
inspect (Wordle { numGuessesAllowed, answer } { currentInput, pastGuesses, history, status }) =
    { numGuessesAllowed = numGuessesAllowed
    , answer = answer
    , currentInput = List.reverse currentInput
    , pastGuesses = pastGuesses
    , history = history
    , status = status
    }
