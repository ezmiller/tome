module Main exposing (..)

import Css exposing (pct, px)
import Html exposing (Html, Attribute, button, text, div, nav, h1, ul, li, a, textarea)
import Html.Attributes exposing (class, href)
import Html.Events exposing (on, onWithOptions, onClick)
import HtmlParser
import HtmlParser.Util
import Http
import Json.Encode exposing (object)
import Json.Decode exposing (Decoder, at, string)
import Json.Decode.Pipeline exposing (..)
import Navbar exposing (navbar, MenuItem, navItem)
import Navigation exposing (Location)
import Routing exposing (Route(..), parseLocation)


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { route : Route
    , editorModel : EditorModel
    , document : String
    }


type Msg
    = SaveDocument
    | DocumentSaved (Result Http.Error DocumentResult)
    | FetchDocument
    | DocumentFetched (Result Http.Error DocumentResult)
    | NotesFetched (Result Http.Error (List DocumentResult))
    | UrlChange Navigation.Location
    | NewUrl String
    | EditorInput String


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initialRoute =
            parseLocation location

        cmd =
            getRouteCmd initialRoute
    in
        ( Model initialRoute initialEditorModel initialDocument
        , Debug.log "initial cmd: " cmd
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveDocument ->
            ( model, saveDoc model.editorModel DocumentSaved )

        DocumentSaved result ->
            ( model, Cmd.none )

        FetchDocument ->
            ( model, Cmd.none )

        DocumentFetched (Ok result) ->
            ( { model | document = result.html }, Cmd.none )

        DocumentFetched (Err error) ->
            ( model, Cmd.none )

        NotesFetched (Ok result) ->
            let
                dummy =
                    Debug.log "notes: " result
            in
                ( model, Cmd.none )

        NotesFetched (Err error) ->
            ( model, Cmd.none )

        EditorInput newVal ->
            ( { model | editorModel = newVal }, Cmd.none )

        UrlChange location ->
            changeLocation location model

        NewUrl url ->
            ( model, Navigation.newUrl url )


changeLocation : Location -> Model -> ( Model, Cmd Msg )
changeLocation location model =
    let
        newRoute =
            parseLocation location

        cmd =
            getRouteCmd newRoute
    in
        ( { model | route = newRoute }, cmd )


getRouteCmd : Route -> Cmd Msg
getRouteCmd route =
    case route of
        Home ->
            fetchNotes NotesFetched

        DocumentViewRoute id ->
            fetchDoc id DocumentFetched

        _ ->
            Cmd.none


menuItems : List (MenuItem Msg)
menuItems =
    [ navItem NewUrl "/" "Home" False
    , navItem NewUrl "/editor" "Edit" False
    ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ navbar menuItems
        , renderCurrentRoute model
        ]


renderCurrentRoute : Model -> Html Msg
renderCurrentRoute model =
    case model.route of
        Home ->
            home model

        Editor ->
            editor model.editorModel

        DocumentViewRoute id ->
            docView model.document id

        NotFoundRoute ->
            text "Not Found!"



-- Home


home : Model -> Html msg
home model =
    text "Home"


fetchNotes : (Result Http.Error (List DocumentResult) -> msg) -> Cmd msg
fetchNotes msg =
    Http.get
        "http://localhost:3000/latest?type=note"
        documentListResponseDecoder
        |> Http.send msg



-- Editor


type alias EditorModel =
    String


initialEditorModel : String
initialEditorModel =
    "Start typing here..."


textAreaStyles : Attribute msg
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
        , button [ class "save-btn", onClick SaveDocument ] [ text "Save" ]
        ]


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string


saveDoc : String -> (Result Http.Error DocumentResult -> msg) -> Cmd msg
saveDoc editorModel msg =
    let
        url =
            "http://localhost:3000/documents"

        data =
            object
                [ ( "doc-string", Json.Encode.string editorModel )
                , ( "doctype", Json.Encode.string "note" )
                ]
    in
        Http.post
            url
            (Http.jsonBody data)
            documentResponseDecoder
            |> Http.send msg


fetchDoc : String -> (Result Http.Error DocumentResult -> msg) -> Cmd msg
fetchDoc docId msg =
    Http.get
        ("http://localhost:3000/documents/" ++ docId)
        documentResponseDecoder
        |> Http.send msg



-- Document View


initialDocument : String
initialDocument =
    "Loading..."


docView : String -> String -> Html msg
docView document docId =
    div []
        (HtmlParser.parse document
            |> HtmlParser.Util.toVirtualDom
        )



-- Decoders


type alias DocumentResult =
    { id : String
    , html : String
    }


type alias DocumentListResult =
    List DocumentResult


documentResponseDecoder : Decoder DocumentResult
documentResponseDecoder =
    Json.Decode.at [ "_embedded" ] documentResultDecoder


documentResultDecoder : Decoder DocumentResult
documentResultDecoder =
    decode DocumentResult
        |> required "id" Json.Decode.string
        |> required "html" Json.Decode.string


documentListResponseDecoder : Decoder (List DocumentResult)
documentListResponseDecoder =
    Json.Decode.at [ "_embedded" ] documentListDecoder


documentListDecoder : Decoder (List DocumentResult)
documentListDecoder =
    Json.Decode.list documentResultDecoder
