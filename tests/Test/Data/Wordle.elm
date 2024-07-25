module Test.Data.Wordle exposing (suite)

import Data.Answer as Answer
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Guess exposing (Guess)
import Data.History as History
import Data.Letter as Letter exposing (Letter)
import Data.Word exposing (Word)
import Data.Wordle as Wordle exposing (Wordle)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Wordle" <|
        List.map
            testWordle
            [ { numGuessesAllowed = 6
              , wordLength = 5
              , answer = "savor"
              , nonAnswers =
                    [ "abort"
                    , "prize"
                    , "rough"
                    ]
              , guesses =
                    [ "prize"
                    , "rough"
                    , "abort"
                    , "savor"
                    ]
              , expected =
                    { pastGuesses =
                        [ [ lc 's', lc 'a', lc 'v', lc 'o', lc 'r' ]
                        , [ lp 'a', li 'b', lp 'o', lp 'r', li 't' ]
                        , [ lp 'r', lp 'o', li 'u', li 'g', li 'h' ]
                        , [ li 'p', lp 'r', li 'i', li 'z', li 'e' ]
                        ]
                    , history =
                        [ ( 'a', hc )
                        , ( 'b', hi )
                        , ( 'e', hi )
                        , ( 'g', hi )
                        , ( 'h', hi )
                        , ( 'i', hi )
                        , ( 'o', hc )
                        , ( 'p', hi )
                        , ( 'r', hc )
                        , ( 's', hc )
                        , ( 't', hi )
                        , ( 'u', hi )
                        , ( 'v', hc )
                        , ( 'z', hi )
                        ]
                    , status = Wordle.Won 4
                    }
              }
            ]


testWordle :
    { numGuessesAllowed : Int
    , wordLength : Int
    , answer : String
    , nonAnswers : List String
    , guesses : List String
    , expected :
        { pastGuesses : List Guess
        , history : List ( Char, History.Past )
        , status : Wordle.Status
        }
    }
    -> Test
testWordle { numGuessesAllowed, wordLength, answer, nonAnswers, guesses, expected } =
    test (Debug.toString { answer = answer, guesses = guesses }) <|
        \_ ->
            let
                dictionary =
                    Dictionary.new
                        { wordLength = wordLength
                        , answers = [ answer ]
                        , nonAnswers = nonAnswers
                        }

                detailsResult =
                    start dictionary numGuessesAllowed answer
                        |> Result.andThen (enterGuesses dictionary guesses)
                        |> Result.map Wordle.inspect
            in
            case detailsResult of
                Ok details ->
                    { numGuessesAllowed = details.numGuessesAllowed
                    , maybeAnswer = Just details.answer
                    , currentInput = details.currentInput
                    , pastGuesses = details.pastGuesses
                    , history = History.toList details.history
                    , status = details.status
                    }
                        |> Expect.equal
                            { numGuessesAllowed = numGuessesAllowed
                            , maybeAnswer = Answer.fromString dictionary answer
                            , currentInput = []
                            , pastGuesses = expected.pastGuesses
                            , history = expected.history
                            , status = expected.status
                            }

                Err error ->
                    Expect.fail <| Debug.toString error


type Error
    = NotAnAnswer String
    | CannotMakeGuessGameOver
    | BadOutcome (Maybe Wordle.Outcome)


start : Dictionary -> Int -> String -> Result Error Wordle
start dictionary numGuessesAllowed possibleAnswer =
    case Answer.fromString dictionary possibleAnswer of
        Just answer ->
            Ok <| Wordle.start numGuessesAllowed answer

        Nothing ->
            Err <| NotAnAnswer possibleAnswer


enterGuesses : Dictionary -> List String -> Wordle -> Result Error Wordle
enterGuesses dictionary guesses wordle =
    case guesses of
        [] ->
            Ok wordle

        guess :: restGuesses ->
            wordle
                |> enterGuess dictionary guess
                |> Result.andThen (enterGuesses dictionary restGuesses)


enterGuess : Dictionary -> String -> Wordle -> Result Error Wordle
enterGuess dictionary guess wordle =
    let
        actions =
            guess
                |> String.toList
                |> List.map Wordle.Input

        ( nextWordle, maybeOutcome ) =
            performActions dictionary (actions ++ [ Wordle.Enter ]) wordle Nothing
    in
    case maybeOutcome of
        Just (Wordle.GuessAllowed word) ->
            makeGuess word nextWordle

        _ ->
            Err <| BadOutcome maybeOutcome


performActions : Dictionary -> List Wordle.Action -> Wordle -> Maybe Wordle.Outcome -> ( Wordle, Maybe Wordle.Outcome )
performActions dictionary actions wordle maybeOutcome =
    case actions of
        [] ->
            ( wordle, maybeOutcome )

        action :: restActions ->
            let
                ( nextWordle, outcome ) =
                    Wordle.perform dictionary action wordle
            in
            performActions dictionary restActions nextWordle (Just outcome)


makeGuess : Word -> Wordle -> Result Error Wordle
makeGuess word wordle =
    let
        { status } =
            Wordle.inspect wordle
    in
    if status == Wordle.InProgress then
        Ok <| Wordle.guess word wordle

    else
        Err <| CannotMakeGuessGameOver


lc : Char -> Letter
lc =
    Letter.Correct


lp : Char -> Letter
lp =
    Letter.Possible


li : Char -> Letter
li =
    Letter.Impossible


hc : History.Past
hc =
    History.Correct


hp : History.Past
hp =
    History.Possible


hi : History.Past
hi =
    History.Impossible
