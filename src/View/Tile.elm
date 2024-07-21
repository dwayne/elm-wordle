module View.Tile exposing (Animation(..), Tile(..), view)

import Html as H
import Html.Attributes as HA
import Lib.Html.Attributes as HA
import Lib.Html.Events as HE
import View.Letter as Letter


type Tile msg
    = Empty
    | NonEmpty Char
    | Correct Char (Animation msg)
    | Possible Char (Animation msg)
    | Impossible Char (Animation msg)


type Animation msg
    = Flip
    | FlipEnd msg


view : Tile msg -> H.Html msg
view tile =
    let
        { modifierClass, onAnimationEnd, front, back } =
            case tile of
                Empty ->
                    { modifierClass = ""
                    , onAnimationEnd = HA.empty
                    , front = [ Letter.view Letter.Empty ]
                    , back = []
                    }

                NonEmpty ch ->
                    { modifierClass = "tile--animation--appear"
                    , onAnimationEnd = HA.empty
                    , front = [ Letter.view <| Letter.NonEmpty ch ]
                    , back = []
                    }

                Correct ch animation ->
                    let
                        animationDetails =
                            toAnimationDetails animation
                    in
                    { modifierClass = animationDetails.modifierClass
                    , onAnimationEnd = animationDetails.onAnimationEnd
                    , front = [ Letter.view <| Letter.NonEmpty ch ]
                    , back = [ Letter.view <| Letter.Correct ch ]
                    }

                Possible ch animation ->
                    let
                        animationDetails =
                            toAnimationDetails animation
                    in
                    { modifierClass = animationDetails.modifierClass
                    , onAnimationEnd = animationDetails.onAnimationEnd
                    , front = [ Letter.view <| Letter.NonEmpty ch ]
                    , back = [ Letter.view <| Letter.Possible ch ]
                    }

                Impossible ch animation ->
                    let
                        animationDetails =
                            toAnimationDetails animation
                    in
                    { modifierClass = animationDetails.modifierClass
                    , onAnimationEnd = animationDetails.onAnimationEnd
                    , front = [ Letter.view <| Letter.NonEmpty ch ]
                    , back = [ Letter.view <| Letter.Impossible ch ]
                    }
    in
    H.div
        [ HA.class "tile"
        , HA.class modifierClass
        , onAnimationEnd
        ]
        [ H.div [ HA.class "tile__front" ] front
        , H.div [ HA.class "tile__back" ] back
        ]


type alias AnimationDetails msg =
    { modifierClass : String
    , onAnimationEnd : H.Attribute msg
    }


toAnimationDetails : Animation msg -> AnimationDetails msg
toAnimationDetails animation =
    case animation of
        Flip ->
            { modifierClass = "tile--animation--flip"
            , onAnimationEnd = HA.empty
            }

        FlipEnd onAnimationEnd ->
            { modifierClass = "tile--animation--flip"
            , onAnimationEnd = HE.onAnimationEnd onAnimationEnd
            }
