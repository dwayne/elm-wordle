module Data.Guess exposing (Guess, guess, isCorrect)

import Data.Answer as Answer exposing (Answer)
import Data.Bag as Bag exposing (Bag)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word exposing (Word)


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
        >> findCorrectLetters { letters = [], chars = chars }
        >> findPossibleLetters


type alias State =
    { letters : List Letter
    , chars : Bag Char
    }


findCorrectLetters : State -> List ( Char, Char ) -> State
findCorrectLetters state pairs =
    case pairs of
        [] ->
            { state | letters = List.reverse state.letters }

        ( t, g ) :: restOfPairs ->
            if t == g then
                findCorrectLetters
                    { state
                        | letters = Letter.Correct g :: state.letters
                        , chars = Bag.remove g state.chars
                    }
                    restOfPairs

            else
                findCorrectLetters
                    { state | letters = Letter.Impossible g :: state.letters }
                    restOfPairs


findPossibleLetters : State -> List Letter
findPossibleLetters { letters, chars } =
    List.map
        (\letterStatus ->
            case letterStatus of
                Letter.Impossible ch ->
                    if Bag.contains ch chars then
                        Letter.Possible ch

                    else
                        letterStatus

                _ ->
                    letterStatus
        )
        letters
