module Main exposing (..)

import Html exposing (Html, text, h1)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)


type alias Model =
    { route : Maybe Route }


type Route
    = Home
    | Editor


type Msg
    = UrlChange Navigation.Location


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Editor (s "editor")
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model (Url.parsePath route location), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        dummy =
            Debug.log "view::model: " model
    in
        case model.route of
            Just route ->
                showRoute route

            Nothing ->
                text "Not Found!"


showRoute : Route -> Html msg
showRoute route =
    case route of
        Home ->
            text "Home"

        Editor ->
            text "Editor"
