module Lib.Html.Attributes exposing (empty)

import Html as H
import Html.Attributes as HA


empty : H.Attribute msg
empty =
    HA.class ""
