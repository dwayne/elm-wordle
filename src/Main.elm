module Main exposing (main)

import Browser
import Browser.Events as BE
import Data.Answer as Answer exposing (Answer)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Dictionary.Classic as Classic
import Data.Guess exposing (Guess)
import Data.History exposing (History)
import Data.Letter as Letter
import Data.Word as Word
import Data.Wordle as Wordle exposing (Wordle)
import Html as H
import Html.Attributes as HA
import Lib.Timer as Timer
import Random
import View.Guess
import View.Guesses
import View.Keyboard
import View.Letter
import View.Message
import View.Title


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
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
    , maybePrevHistory : Maybe History
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
    | RevealEnded
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
                                , maybePrevHistory = Nothing
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
                        ( wordle1, outcome ) =
                            Wordle.perform dictionary (keyToAction key) state.wordle
                    in
                    case outcome of
                        Wordle.AppendChar _ ->
                            ( Loaded { state | wordle = wordle1 }
                            , Cmd.none
                            )

                        Wordle.RemoveChar _ ->
                            ( Loaded { state | wordle = wordle1 }
                            , Cmd.none
                            )

                        Wordle.GuessAllowed word ->
                            let
                                wordle2 =
                                    Wordle.guess word wordle1

                                { history } =
                                    Wordle.inspect wordle1
                            in
                            ( Loaded
                                { state
                                    | wordle = wordle2
                                    , maybePrevHistory = Just history
                                }
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

                RevealEnded ->
                    let
                        { answer, pastGuesses, status } =
                            Wordle.inspect state.wordle
                    in
                    ( case status of
                        Wordle.InProgress ->
                            Loaded { state | maybePrevHistory = Nothing }

                        Wordle.Won numGuesses ->
                            Loaded { state | isOpen = True, message = toWonMessage numGuesses }

                        Wordle.Lost ->
                            Loaded { state | isOpen = True, message = toLostMessage answer }
                    , Cmd.none
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


toWonMessage : Int -> String
toWonMessage n =
    if n == numGuessesAllowed then
        "Phew"

    else if n <= 1 then
        "Genius"

    else if n == 2 then
        "Magnificent"

    else if n == 3 then
        "Impressive"

    else if n == 4 then
        "Splendid"

    else
        "Great"


toLostMessage : Answer -> String
toLostMessage =
    String.toUpper << Answer.toString



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Loaded { isOpen, shake, maybePrevHistory } ->
            let
                isDisabled =
                    isOpen || shake || maybePrevHistory /= Nothing
            in
            if isDisabled then
                Sub.none

            else
                BE.onKeyDown <| View.Keyboard.decoder KeyPressed

        NoAnswer ->
            Sub.none



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
viewWordle { wordle, isOpen, message, shake, maybePrevHistory } =
    let
        details =
            Wordle.inspect wordle

        ( maybeAction, history ) =
            case maybePrevHistory of
                Just prevHistory ->
                    ( Just <| View.Guesses.Reveal RevealEnded, prevHistory )

                Nothing ->
                    ( if shake then
                        Just <| View.Guesses.Shake ShakeEnded

                      else
                        Nothing
                    , details.history
                    )

        isDisabled =
            isOpen || maybeAction /= Nothing
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
                [ View.Guesses.view
                    { numGuessesAllowed = numGuessesAllowed
                    , wordLength = wordLength
                    , pastGuesses = details.pastGuesses
                    , currentInput = details.currentInput
                    , maybeAction = maybeAction
                    }
                ]
            , H.div [ HA.class "wordle__keyboard" ]
                [ View.Keyboard.view
                    { history = history
                    , maybeOnKeyPress =
                        if isDisabled then
                            Nothing

                        else
                            Just KeyPressed
                    }
                ]
            ]
        ]
