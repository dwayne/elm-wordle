module View.Guess exposing (State(..), ViewOptions, viewCurrent, viewEmpty, viewPast)

import Data.Guess exposing (Guess)
import Data.Letter as Letter exposing (Letter)
import Html as H
import Html.Attributes as HA
import Lib.Html.Attributes as HA
import Lib.Html.Events as HE
import View.Letter
import View.Tile exposing (Tile)


viewEmpty : Int -> H.Html msg
viewEmpty wordLength =
    H.div [ HA.class "guess" ] <|
        List.repeat wordLength (View.Letter.view View.Letter.Empty)


viewPast : Guess -> H.Html msg
viewPast =
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


type alias ViewOptions msg =
    { wordLength : Int
    , state : State msg
    }


type State msg
    = InProgress
        { input : List Char
        , maybeOnShakeEnd : Maybe msg
        }
    | Completed
        { guess : Guess
        , onRevealEnd : msg
        }


viewCurrent : ViewOptions msg -> H.Html msg
viewCurrent { wordLength, state } =
    case state of
        InProgress { input, maybeOnShakeEnd } ->
            let
                ( modifierClass, onShakeEnd ) =
                    case maybeOnShakeEnd of
                        Just msg ->
                            ( "guess--animation--shake"
                            , HE.onAnimationEnd msg
                            )

                        Nothing ->
                            ( ""
                            , HA.empty
                            )

                tiles =
                    input
                        |> charsToTiles wordLength
                        |> List.map View.Tile.view
            in
            H.div
                [ HA.class "guess"
                , HA.class modifierClass
                , onShakeEnd
                ]
                tiles

        Completed { guess, onRevealEnd } ->
            let
                tiles =
                    guess
                        |> lettersToTiles [] onRevealEnd
                        |> List.map View.Tile.view
            in
            H.div [ HA.class "guess" ] tiles


charsToTiles : Int -> List Char -> List (Tile msg)
charsToTiles wordLength chars =
    let
        numEmptyTiles =
            wordLength - List.length chars
    in
    List.concat
        [ List.map View.Tile.NonEmpty chars
        , List.repeat numEmptyTiles View.Tile.Empty
        ]


lettersToTiles : List (Tile msg) -> msg -> List Letter -> List (Tile msg)
lettersToTiles revTiles onRevealEnd letters =
    case letters of
        [] ->
            List.reverse revTiles

        [ letter ] ->
            lettersToTiles
                (letterToTile letter (View.Tile.Flip onRevealEnd) :: revTiles)
                onRevealEnd
                []

        letter :: restLetters ->
            lettersToTiles
                (letterToTile letter View.Tile.None :: revTiles)
                onRevealEnd
                restLetters


letterToTile : Letter -> View.Tile.Animation msg -> Tile msg
letterToTile letter animation =
    case letter of
        Letter.Correct ch ->
            View.Tile.Correct ch animation

        Letter.Possible ch ->
            View.Tile.Possible ch animation

        Letter.Impossible ch ->
            View.Tile.Impossible ch animation
