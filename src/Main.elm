module Main exposing (main)

import Browser
import Html as H


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd msg )
init _ =
    ( {}
    , Cmd.none
    )


-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


-- VIEW


view : Model -> H.Html msg
view _ =
    H.text "Hello, world!"
