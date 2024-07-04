module View.Letter exposing (Letter(..), view)

import Html as H
import Html.Attributes as HA


type Letter
    = Empty
    | NonEmpty Char
    | Correct Char
    | Possible Char
    | Impossible Char


view : Letter -> H.Html msg
view letter =
    let
        ( modifierClass, text ) =
            case letter of
                Empty ->
                    ( "", "" )

                NonEmpty ch ->
                    ( "", String.fromChar ch )

                Correct ch ->
                    ( "letter--result--correct", String.fromChar ch )

                Possible ch ->
                    ( "letter--result--possible", String.fromChar ch )

                Impossible ch ->
                    ( "letter--result--impossible", String.fromChar ch )
    in
    H.div
        [ HA.class "letter"
        , HA.class modifierClass
        ]
        [ H.text text ]
