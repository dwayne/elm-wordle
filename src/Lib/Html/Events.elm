module Lib.Html.Events exposing (onAnimationEnd)

import Html as H
import Html.Events as HE
import Json.Decode as JD


onAnimationEnd : msg -> H.Attribute msg
onAnimationEnd =
    HE.on "animationend" << JD.succeed
