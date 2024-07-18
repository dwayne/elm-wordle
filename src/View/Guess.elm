module View.Guess exposing (viewEmpty, viewPast)

import Data.Guess exposing (Guess)
import Data.Letter as Letter
import Html as H
import Html.Attributes as HA
import View.Letter


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
