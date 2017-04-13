module Main exposing (..)

import Html exposing (Html, text, div, h1, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Json.Decode exposing (at, string)


type alias Model =
    { route : Maybe Route
    , editorModel : EditorModel
    }


type Route
    = Home
    | Editor


type Msg
    = UrlChange Navigation.Location
    | EditorInput String


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
    ( Model (Url.parsePath route location) initialEditorModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorInput newVal ->
            ( { model | editorModel = (Debug.log "newVal: " newVal) }, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        dummy =
            Debug.log "view::model: " model
    in
        case model.route of
            Just route ->
                showRoute route model

            Nothing ->
                text "Not Found!"


showRoute : Route -> Model -> Html Msg
showRoute route model =
    case route of
        Home ->
            text "Home"

        Editor ->
            editor model.editorModel



-- Editor


type alias EditorModel =
    String


initialEditorModel : String
initialEditorModel =
    "Start typing here..."


editor : EditorModel -> Html Msg
editor editorModel =
    div
        [ class "editor" ]
        [ textarea
            [ on "input" (Json.Decode.map EditorInput valueDecoder) ]
            [ text editorModel ]
        ]


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string
