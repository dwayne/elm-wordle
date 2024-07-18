module View.Keyboard exposing (Key(..), ViewOptions, view)

import Data.History as History exposing (History)
import Html as H
import Html.Attributes as HA
import View.Key


type alias ViewOptions msg =
    { history : History
    , maybeOnKeyPress : Maybe (Key -> msg)
    }


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


view : ViewOptions msg -> H.Html msg
view options =
    H.div [ HA.class "keyboard" ]
        [ viewRow options "qwertyyuiop"
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
