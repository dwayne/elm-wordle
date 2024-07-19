module View.Message exposing (view)

import Html as H
import Html.Attributes as HA


view : String -> H.Html msg
view message =
    if String.isEmpty message then
        H.text ""

    else
        H.div [ HA.class "message" ] [ H.text message ]
