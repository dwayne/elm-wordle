module Lib.Timer exposing (setTimeout)

import Process
import Task


setTimeout : Float -> msg -> Cmd msg
setTimeout ms msg =
    Process.sleep ms
        |> Task.perform (always msg)
