module View.Key exposing (Key(..), ViewOptions, view)

import Data.History as History
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA


type alias ViewOptions msg =
    { key : Key
    , maybeOnClick : Maybe msg
    }


type Key
    = Default (Maybe History.Past) Char
    | Enter
    | Delete


view : ViewOptions msg -> H.Html msg
view { key, maybeOnClick } =
    let
        ( modifierClass, content ) =
            case key of
                Default maybePast ch ->
                    ( case maybePast of
                        Just past ->
                            case past of
                                History.Correct ->
                                    "key--past--correct"

                                History.Possible ->
                                    "key--past--possible"

                                History.Impossible ->
                                    "key--past--impossible"

                        Nothing ->
                            ""
                    , H.text <| String.fromChar ch
                    )

                Enter ->
                    ( "key--large"
                    , H.text "Enter"
                    )

                Delete ->
                    ( "key--large"
                    , viewDelete
                    )
    in
    H.button
        [ HA.class "key"
        , HA.class modifierClass
        , case maybeOnClick of
            Just onClick ->
                HE.onClick onClick

            Nothing ->
                HA.disabled True
        ]
        [ content ]


viewDelete : H.Html msg
viewDelete =
    S.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width "24"
        , SA.height "24"
        ]
        [ S.path
            [ SA.fill "currentColor"
            , SA.d "M22 3H7c-.69 0-1.23.35-1.59.88L0 12l5.41 8.11c.36.53.9.89 1.59.89h15c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H7.07L2.4 12l4.66-7H22v14zm-11.59-2L14 13.41 17.59 17 19 15.59 15.41 12 19 8.41 17.59 7 14 10.59 10.41 7 9 8.41 12.59 12 9 15.59z"
            ]
            []
        ]
