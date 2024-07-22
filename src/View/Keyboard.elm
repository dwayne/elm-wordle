module View.Keyboard exposing (Key(..), ViewOptions, decoder, view)

import Data.History as History exposing (History)
import Html as H
import Html.Attributes as HA
import Json.Decode as JD
import View.Key


type
    Key
    --
    -- I want to indicate which key was pressed but I don't
    -- want the "past" information that comes along with the
    -- View.Key.Character data constructor. Hence the reason
    -- for a new Key type that excludes that data.
    --
    = Character Char
    | Enter
    | Delete


decoder : (Key -> msg) -> JD.Decoder msg
decoder toMsg =
    let
        keyDecoder =
            JD.string
                |> JD.andThen
                    (\s ->
                        case s of
                            "Enter" ->
                                JD.succeed <| toMsg Enter

                            "Backspace" ->
                                JD.succeed <| toMsg Delete

                            _ ->
                                case String.uncons s of
                                    Just ( ch, "" ) ->
                                        if Char.isAlpha ch then
                                            JD.succeed <| toMsg <| Character ch

                                        else
                                            JD.fail "ignored"

                                    _ ->
                                        JD.fail "ignored"
                    )
    in
    JD.field "key" keyDecoder



-- VIEW


type alias ViewOptions msg =
    { history : History
    , maybeOnKeyPress : Maybe (Key -> msg)
    }


view : ViewOptions msg -> H.Html msg
view options =
    H.div [ HA.class "keyboard" ]
        [ viewRow options "qwertyuiop"
        , viewRow options "asdfghjkl"
        , viewRow options "+zxcvbnm-"
        ]


viewRow : ViewOptions msg -> String -> H.Html msg
viewRow { history, maybeOnKeyPress } =
    String.toList
        >> List.map
            (\ch ->
                case ch of
                    '+' ->
                        View.Key.view
                            { key = View.Key.Enter
                            , maybeOnClick = Maybe.map ((|>) Enter) maybeOnKeyPress
                            }

                    '-' ->
                        View.Key.view
                            { key = View.Key.Delete
                            , maybeOnClick = Maybe.map ((|>) Delete) maybeOnKeyPress
                            }

                    _ ->
                        let
                            past =
                                History.getPast ch history
                        in
                        View.Key.view
                            { key = View.Key.Character past ch
                            , maybeOnClick = Maybe.map ((|>) (Character ch)) maybeOnKeyPress
                            }
            )
        >> H.div [ HA.class "keyboard__row" ]
