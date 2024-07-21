module View.Guesses exposing (Action(..), ViewOptions, view)

import Data.Guess exposing (Guess)
import Html as H
import Html.Attributes as HA
import View.Guess


type alias ViewOptions msg =
    { numGuessesAllowed : Int
    , wordLength : Int
    , pastGuesses : List Guess
    , currentInput : List Char
    , maybeAction : Maybe (Action msg)
    }


type Action msg
    = Shake msg
    | Reveal msg


view : ViewOptions msg -> H.Html msg
view { numGuessesAllowed, wordLength, pastGuesses, currentInput, maybeAction } =
    let
        ( guesses, state ) =
            case maybeAction of
                Nothing ->
                    ( List.reverse pastGuesses
                    , View.Guess.InProgress
                        { input = currentInput
                        , maybeOnShakeEnd = Nothing
                        }
                    )

                Just (Shake onShakeEnd) ->
                    ( List.reverse pastGuesses
                    , View.Guess.InProgress
                        { input = currentInput
                        , maybeOnShakeEnd = Just onShakeEnd
                        }
                    )

                Just (Reveal onRevealEnd) ->
                    case pastGuesses of
                        [] ->
                            ( []
                            , View.Guess.InProgress
                                { input = currentInput
                                , maybeOnShakeEnd = Nothing
                                }
                            )

                        guess :: restPastGuesses ->
                            ( List.reverse restPastGuesses
                            , View.Guess.Completed
                                { guess = guess
                                , onRevealEnd = onRevealEnd
                                }
                            )

        numEmptyRows =
            numGuessesAllowed - List.length guesses - 1
    in
    H.div [ HA.class "guesses" ] <|
        List.concat
            [ List.map View.Guess.viewPast guesses
            , if numEmptyRows >= 0 then
                [ View.Guess.viewCurrent
                    { wordLength = wordLength
                    , state = state
                    }
                ]

              else
                []
            , List.repeat numEmptyRows (View.Guess.viewEmpty wordLength)
            ]
