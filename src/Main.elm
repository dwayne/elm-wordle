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
import View.Guess
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
    | Loaded Wordle
    | Error String


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
                            ( Loaded <| Wordle.start numGuessesAllowed answer
                            , Cmd.none
                            )

                        Nothing ->
                            ( Error "Unable to generate an answer."
                            , Cmd.none
                            )

                KeyPressed _ ->
                    ( model, Cmd.none )

        Loaded wordle ->
            case msg of
                GeneratedMaybeAnswer _ ->
                    ( model, Cmd.none )

                KeyPressed key ->
                    let
                        ( newWordle, _ ) =
                            Wordle.perform dictionary (keyToAction key) wordle
                    in
                    ( Loaded newWordle, Cmd.none )

        Error _ ->
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

        Loaded wordle ->
            viewWordle wordle

        Error error ->
            H.text error


viewWordle : Wordle -> H.Html Msg
viewWordle wordle =
    let
        { currentInput, pastGuesses, history } =
            Wordle.inspect wordle
    in
    H.div [ HA.class "wordle" ]
        [ H.header [ HA.class "wordle__header" ] [ View.Title.view ]
        , H.main_ []
            [ H.div [ HA.class "wordle__guesses" ]
                [ viewGuesses
                    { pastGuesses = pastGuesses
                    , currentInput = currentInput
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
    { pastGuesses : List Guess
    , currentInput : List Char
    }


viewGuesses : ViewGuessesOptions -> H.Html msg
viewGuesses { pastGuesses, currentInput } =
    let
        numEmptyRows =
            numGuessesAllowed - List.length pastGuesses - 1
    in
    H.div [ HA.class "guesses" ] <|
        List.concat
            [ List.map View.Guess.viewPast pastGuesses
            , if numEmptyRows >= 0 then
                [ viewCurrentGuess currentInput ]

              else
                []
            , List.repeat numEmptyRows (View.Guess.viewEmpty wordLength)
            ]


viewCurrentGuess : List Char -> H.Html msg
viewCurrentGuess current =
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
