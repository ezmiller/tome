module Main exposing (..)

import Css exposing (pct, px)
import Html exposing (Html, Attribute, button, text, div, nav, h1, ul, li, a, textarea)
import Html.Attributes exposing (class, href)
import Html.Events exposing (on, onWithOptions, onClick)
import Json.Decode exposing (at, string)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { route : Maybe Route
    , editorModel : EditorModel
    }


type Route
    = Home
    | Editor


type Msg
    = Save
    | UrlChange Navigation.Location
    | NewUrl String
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
        Save ->
            ( model, saveDoc model.editorModel )

        EditorInput newVal ->
            ( { model | editorModel = (Debug.log "newVal: " newVal) }, Cmd.none )

        UrlChange location ->
            ( { model | route = Url.parsePath route location }
            , Cmd.none
            )

        NewUrl url ->
            ( model, Navigation.newUrl url )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ navbar
        , renderCurrentRoute model
        ]


renderCurrentRoute : Model -> Html Msg
renderCurrentRoute model =
    case model.route of
        Just Home ->
            text "Home"

        Just Editor ->
            editor model.editorModel

        Nothing ->
            text "Not Found!"



-- Navbar


type MenuItem msg
    = MenuItem
        { attributes : List (Attribute msg)
        , children : List (Html msg)
        }


itemLink : List (Attribute msg) -> List (Html msg) -> MenuItem msg
itemLink attributes children =
    MenuItem
        { attributes = attributes
        , children = children
        }


navItem : String -> String -> Bool -> MenuItem Msg
navItem url label selected =
    itemLink
        [ href url
        , onWithOptions "click"
            { stopPropagation = True, preventDefault = True }
            (Json.Decode.succeed (NewUrl url))
        ]
        [ text label ]


menuItems : List (MenuItem Msg)
menuItems =
    [ navItem "/" "Home" False
    , navItem "/editor" "Edit" False
    ]


navbar : Html Msg
navbar =
    nav []
        [ div [] [ renderItems menuItems ] ]


renderItem : MenuItem msg -> Html msg
renderItem (MenuItem { attributes, children }) =
    li [ class "nav-item" ]
        [ a ([ class "nav-link" ] ++ attributes) children ]


renderItems : List (MenuItem msg) -> Html msg
renderItems items =
    ul [] (List.map renderItem items)



-- Editor


type alias EditorModel =
    String


initialEditorModel : String
initialEditorModel =
    "Start typing here..."


textAreaStyles =
    styles
        [ Css.width (pct 100)
        , Css.height (px 600)
        , Css.padding (px 15)
        , Css.fontSize (px 18)
        ]


editor : EditorModel -> Html Msg
editor editorModel =
    div
        [ class "editor" ]
        [ textarea
            [ on "input" (Json.Decode.map EditorInput valueDecoder)
            , textAreaStyles
            ]
            [ text editorModel ]
        , button [ class "save-btn", onClick Save ] [ text "Save" ]
        ]


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string


saveDoc : String -> Cmd Msg
saveDoc editorModel =
    let
        dummy =
            Debug.log "saveDoc: " editorModel
    in
        Cmd.none
