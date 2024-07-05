module Main exposing (main)

import Browser
import Data.Answer as Answer exposing (Answer)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Dictionary.Classic as Classic
import Data.Guess exposing (Guess)
import Data.Letter as Letter
import Data.Word as Word
import Data.Wordle as Wordle exposing (Wordle)
import Html as H
import Html.Attributes as HA
import Random
import View.Keyboard
import View.Letter
import View.Title


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type Model
    = Loading
    | InProgress InProgressModel
    | Error String


type alias InProgressModel =
    { wordle : Wordle
    , current : List Char
    }


--
-- N.B. I removed the dictionary from within the model
-- since it is really large and it seems to be causing
-- problems with the debugger when messages are generated
-- and/or the model is changed. I was getting a
-- TOO MUCH RECURSION error from the JavaScript.
--
dictionary : Dictionary
dictionary =
    Classic.dictionary


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GeneratedMaybeAnswer (Answer.generator dictionary)
    )



-- UPDATE


type Msg
    = GeneratedMaybeAnswer (Maybe Answer)
    | KeyPressed View.Keyboard.Key


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Loading ->
            case msg of
                GeneratedMaybeAnswer maybeAnswer ->
                    case maybeAnswer of
                        Just answer ->
                            ( InProgress
                                --
                                -- TODO: Remove hardcoded numGuessesAllowed.
                                --
                                { wordle = Wordle.start 6 answer
                                , current = []
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( Error "Unable to generate an answer."
                            , Cmd.none
                            )

                KeyPressed _ ->
                    ( model, Cmd.none )

        InProgress inProgressModel ->
            case msg of
                GeneratedMaybeAnswer _ ->
                    ( model, Cmd.none )

                KeyPressed key ->
                    case key of
                        View.Keyboard.Letter ch ->
                            ( InProgress { inProgressModel | current = inProgressModel.current ++ [ ch ] }
                            , Cmd.none
                            )

                        View.Keyboard.Enter ->
                            ( model, Cmd.none )

                        View.Keyboard.Delete ->
                            ( model, Cmd.none )

        Error _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> H.Html Msg
view model =
    case model of
        Loading ->
            H.text "Loading..."

        InProgress inProgressModel ->
            viewWordle inProgressModel

        Error error ->
            H.text error


viewWordle : InProgressModel -> H.Html Msg
viewWordle { wordle, current } =
    let
        wordLength =
            Dictionary.toWordLength dictionary

        { numGuessesAllowed, pastGuesses, history } =
            Wordle.inspect wordle
    in
    H.div [ HA.class "wordle" ]
        [ H.header [ HA.class "wordle__header" ] [ View.Title.view ]
        , H.main_ []
            [ H.div [ HA.class "wordle__guesses" ]
                [ viewGuesses
                    { wordLength = wordLength
                    , numGuessesAllowed = numGuessesAllowed
                    , pastGuesses = pastGuesses
                    , current = current
                    }
                ]
            , H.div [ HA.class "wordle__keyboard" ]
                [ View.Keyboard.view
                    { history = history
                    , maybeOnKeyPress = Just KeyPressed
                    }
                ]
            ]
        ]


type alias ViewGuessesOptions =
    { wordLength : Int
    , numGuessesAllowed : Int
    , pastGuesses : List Guess
    , current : List Char
    }


viewGuesses : ViewGuessesOptions -> H.Html msg
viewGuesses { wordLength, numGuessesAllowed, pastGuesses, current } =
    let
        numEmptyRows =
            numGuessesAllowed - List.length pastGuesses - 1
    in
    H.div [ HA.class "guesses" ] <|
        List.concat
            [ List.map viewPastGuess pastGuesses
            , if numEmptyRows >= 0 then
                [ viewCurrentGuess wordLength current ]

              else
                []
            , List.repeat numEmptyRows (viewEmptyGuess wordLength)
            ]


viewPastGuess : Guess -> H.Html msg
viewPastGuess =
    List.map
        (\letter ->
            case letter of
                Letter.Correct ch ->
                    View.Letter.view <| View.Letter.Correct ch

                Letter.Possible ch ->
                    View.Letter.view <| View.Letter.Possible ch

                Letter.Impossible ch ->
                    View.Letter.view <| View.Letter.Impossible ch
        )
        >> H.div [ HA.class "guess" ]


viewCurrentGuess : Int -> List Char -> H.Html msg
viewCurrentGuess wordLength current =
    let
        numEmptyTiles =
            wordLength - List.length current
    in
    H.div [ HA.class "guess" ] <|
        List.concat
            [ List.map viewTile current
            , List.repeat numEmptyTiles viewEmptyTile
            ]


viewTile : Char -> H.Html msg
viewTile ch =
    H.div [ HA.class "tile tile--animation--appear" ]
        [ H.div [ HA.class "tile__front" ]
            [ View.Letter.view <| View.Letter.NonEmpty ch ]
        , H.div [ HA.class "tile__back" ] []
        ]


viewEmptyTile : H.Html msg
viewEmptyTile =
    H.div [ HA.class "tile" ]
        [ H.div [ HA.class "tile__front" ]
            [ View.Letter.view View.Letter.Empty ]
        , H.div [ HA.class "tile__back" ] []
        ]


viewEmptyGuess : Int -> H.Html msg
viewEmptyGuess wordLength =
    H.div [ HA.class "guess" ] <|
        List.repeat wordLength (View.Letter.view View.Letter.Empty)
