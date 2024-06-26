module Data.Attempt exposing (Attempt, isCorrect, try)

import Data.Answer as Answer exposing (Answer)
import Data.Bag as Bag exposing (Bag)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word exposing (Word)


type alias Attempt =
    List Letter


isCorrect : Attempt -> Bool
isCorrect =
    List.all Letter.isCorrect


try : Maybe (Bag Char) -> Answer -> Word -> Attempt
try maybeCharsInAnswers answer =
    let
        chars =
            case maybeCharsInAnswers of
                Just charsInAnswers ->
                    charsInAnswers

                Nothing ->
                    Answer.toChars answer
    in
    Answer.zip answer
        >> findCorrectGuesses { letters = [], chars = chars }
        >> refineIncorrectGuesses


type alias State =
    { letters : List Letter
    , chars : Bag Char
    }


findCorrectGuesses : State -> List ( Char, Char ) -> State
findCorrectGuesses state pairs =
    case pairs of
        [] ->
            { state | letters = List.reverse state.letters }

        ( t, g ) :: restOfPairs ->
            if t == g then
                findCorrectGuesses
                    { state
                        | letters = Letter.Correct g :: state.letters
                        , chars = Bag.remove g state.chars
                    }
                    restOfPairs

            else
                findCorrectGuesses
                    { state | letters = Letter.Incorrect g :: state.letters }
                    restOfPairs


refineIncorrectGuesses : State -> List Letter
refineIncorrectGuesses { letters, chars } =
    List.map
        (\letterStatus ->
            case letterStatus of
                Letter.Incorrect ch ->
                    if Bag.contains ch chars then
                        Letter.AlmostCorrect ch

                    else
                        letterStatus

                _ ->
                    letterStatus
        )
        letters
