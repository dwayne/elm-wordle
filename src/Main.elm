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
import Lib.Timer as Timer
import Random
import View.Guess
import View.Keyboard
import View.Letter
import View.Message
import View.Title


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- CONFIG


numGuessesAllowed : Int
numGuessesAllowed =
    6


dictionary : Dictionary
dictionary =
    Classic.dictionary


wordLength : Int
wordLength =
    Dictionary.toWordLength dictionary



-- MODEL


type Model
    = Loading
    | Loaded State
    | NoAnswer


type alias State =
    { wordle : Wordle
    , isOpen : Bool
    , message : String
    , shake : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GeneratedMaybeAnswer (Answer.generator dictionary)
    )



-- UPDATE


type Msg
    = GeneratedMaybeAnswer (Maybe Answer)
    | KeyPressed View.Keyboard.Key
    | ShakeEnded
    | MessageClosed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                GeneratedMaybeAnswer maybeAnswer ->
                    case maybeAnswer of
                        Just answer ->
                            ( Loaded
                                { wordle = Wordle.start numGuessesAllowed answer
                                , isOpen = False
                                , message = ""
                                , shake = False
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( NoAnswer
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Loaded state ->
            case msg of
                KeyPressed key ->
                    let
                        ( newWordle, outcome ) =
                            Wordle.perform dictionary (keyToAction key) state.wordle
                    in
                    case outcome of
                        Wordle.AppendChar _ ->
                            ( Loaded { state | wordle = newWordle }
                            , Cmd.none
                            )

                        Wordle.RemoveChar _ ->
                            ( Loaded { state | wordle = newWordle }
                            , Cmd.none
                            )

                        Wordle.GuessAllowed _ ->
                            ( Loaded { state | wordle = newWordle }
                            , Cmd.none
                            )

                        Wordle.NoChange reason ->
                            case reason of
                                Wordle.Empty ->
                                    ( model, Cmd.none )

                                Wordle.GameOver ->
                                    ( model, Cmd.none )

                                Wordle.MaxLength _ ->
                                    ( model, Cmd.none )

                                Wordle.NotAWord _ ->
                                    ( Loaded
                                        { state
                                            | isOpen = True
                                            , message = "Not in word list"
                                            , shake = True
                                        }
                                    , Cmd.none
                                    )

                                Wordle.NotEnoughLetters _ ->
                                    ( Loaded
                                        { state
                                            | isOpen = True
                                            , message = "Not enough letters"
                                            , shake = True
                                        }
                                    , Cmd.none
                                    )

                ShakeEnded ->
                    ( Loaded { state | shake = False }
                    , Timer.setTimeout 750 MessageClosed
                    )

                MessageClosed ->
                    ( Loaded { state | isOpen = False }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NoAnswer ->
            ( model, Cmd.none )


keyToAction : View.Keyboard.Key -> Wordle.Action
keyToAction key =
    case key of
        View.Keyboard.Character ch ->
            Wordle.Input ch

        View.Keyboard.Enter ->
            Wordle.Enter

        View.Keyboard.Delete ->
            Wordle.Delete



-- VIEW


view : Model -> H.Html Msg
view model =
    case model of
        Loading ->
            H.text "Loading..."

        Loaded state ->
            viewWordle state

        NoAnswer ->
            H.text "Unable to generate an answer."


viewWordle : State -> H.Html Msg
viewWordle { wordle, isOpen, message, shake } =
    let
        { currentInput, pastGuesses, history } =
            Wordle.inspect wordle
    in
    H.div [ HA.class "wordle" ]
        [ H.div
            [ HA.class "wordle__message"
            , HA.classList [ ( "wordle__message--open", isOpen ) ]
            ]
            [ View.Message.view message
            ]
        , H.header [ HA.class "wordle__header" ] [ View.Title.view ]
        , H.main_ []
            [ H.div [ HA.class "wordle__guesses" ]
                [ viewGuesses
                    { maybeOnShakeEnd =
                        if shake then
                            Just ShakeEnded

                        else
                            Nothing
                    , currentInput = currentInput
                    , pastGuesses = pastGuesses
                    }
                ]
            , H.div [ HA.class "wordle__keyboard" ]
                [ View.Keyboard.view
                    { history = history
                    , maybeOnKeyPress =
                        if isOpen then
                            Nothing

                        else
                            Just KeyPressed
                    }
                ]
            ]
        ]


type alias ViewGuessesOptions msg =
    { maybeOnShakeEnd : Maybe msg
    , pastGuesses : List Guess
    , currentInput : List Char
    }


viewGuesses : ViewGuessesOptions msg -> H.Html msg
viewGuesses { maybeOnShakeEnd, pastGuesses, currentInput } =
    let
        numEmptyRows =
            numGuessesAllowed - List.length pastGuesses - 1
    in
    H.div [ HA.class "guesses" ] <|
        List.concat
            [ List.map View.Guess.viewPast pastGuesses
            , if numEmptyRows >= 0 then
                [ View.Guess.viewCurrent
                    { wordLength = wordLength
                    , state =
                        View.Guess.InProgress
                            { input = currentInput
                            , maybeOnShakeEnd = maybeOnShakeEnd
                            }
                    }
                ]

              else
                []
            , List.repeat numEmptyRows (View.Guess.viewEmpty wordLength)
            ]
