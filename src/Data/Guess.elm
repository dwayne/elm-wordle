module Data.Guess exposing (Guess, guess, isCorrect)

import Data.Answer as Answer exposing (Answer)
import Data.Bag as Bag exposing (Bag)
import Data.Letter as Letter exposing (Letter)
import Data.Word exposing (Word)


type alias Guess =
    List Letter


isCorrect : Guess -> Bool
isCorrect =
    List.all Letter.isCorrect


guess : Maybe (Bag Char) -> Answer -> Word -> Guess
guess maybeCharsInAnswers answer =
    let
        chars =
            case maybeCharsInAnswers of
                Just charsInAnswers ->
                    charsInAnswers

                Nothing ->
                    Answer.toChars answer
    in
    Answer.zip answer
        >> findCorrectLetters chars
        >> findPossibleLetters


findCorrectLetters : Bag Char -> List ( Char, Char ) -> State
findCorrectLetters chars =
    List.foldl
        (\( a, g ) state ->
            if a == g then
                state
                    |> appendLetter (Letter.Correct g)
                    |> removeChar g

            else
                state
                    |> appendLetter (Letter.Impossible g)
        )
        (initState chars)


findPossibleLetters : State -> List Letter
findPossibleLetters { letters, chars } =
    letters
        |> List.foldl
            (\letter state ->
                case letter of
                    Letter.Impossible ch ->
                        if Bag.contains ch state.chars then
                            state
                                |> appendLetter (Letter.Possible ch)
                                |> removeChar ch

                        else
                            appendLetter letter state

                    _ ->
                        appendLetter letter state
            )
            (initState chars)
        |> .letters



-- STATE


type alias State =
    { letters : List Letter
    , chars : Bag Char
    }


initState : Bag Char -> State
initState chars =
    { letters = [], chars = chars }


appendLetter : Letter -> State -> State
appendLetter letter state =
    { state | letters = state.letters ++ [ letter ] }


removeChar : Char -> State -> State
removeChar ch state =
    { state | chars = Bag.remove ch state.chars }
