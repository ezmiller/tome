module Main exposing (..)

import Css exposing (pct, px)
import Html
    exposing
        ( Html
        , Attribute
        , button
        , text
        , div
        , header
        , nav
        , h1
        , input
        , ul
        , li
        , a
        , textarea
        )
import Html.Attributes exposing (class, href, placeholder, value)
import HtmlParser
import HtmlParser.Util
import Http
import Json.Decode exposing (Decoder, at, string)
import Json.Decode.Pipeline exposing (..)
import Navbar exposing (navbar, MenuItem, navItem)
import Navigation exposing (Location)
import Routing exposing (Route(..), parseLocation)


baseUrl : String
baseUrl =
    "http://localhost:6789"


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { route : Route
    , document : Document
    , notes : List Document
    }


type Msg
    = FetchDocument
    | DocumentFetched (Result Http.Error Document)
    | NotesFetched (Result Http.Error (List Document))
    | UrlChange Navigation.Location
    | NewUrl String


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
        ( Model initialRoute initialDocument initialNotes
        , Debug.log "initial cmd: " cmd
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDocument ->
            ( model, Cmd.none )

        DocumentFetched (Ok document) ->
            ( { model | document = document }, Cmd.none )

        DocumentFetched (Err error) ->
            let
                dummy =
                    Debug.log "error: " error
            in
                ( model, Cmd.none )

        NotesFetched (Ok result) ->
            ( { model | notes = result }, Cmd.none )

        NotesFetched (Err error) ->
            let
                dummy =
                    Debug.log "error: " error
            in
                ( model, Cmd.none )

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
    [ navItem NewUrl "/" "Home" False ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ pageHeader
        , renderCurrentRoute model
        ]


renderCurrentRoute : Model -> Html Msg
renderCurrentRoute model =
    case model.route of
        Home ->
            home model

        DocumentViewRoute id ->
            docView model.document id

        NotFoundRoute ->
            text "Not Found!"



-- Header


pageHeader : Html msg
pageHeader =
    header [] [ h1 [ class "site-title" ] [ text "HumansCode / Open Notebook" ] ]



-- Home


initialNotes : List Document
initialNotes =
    []


home : Model -> Html msg
home model =
    div [ class "recent-notes" ] (List.map renderNoteLink model.notes)


renderNoteLink : { a | id : String, title : String } -> Html msg
renderNoteLink note =
    li []
        [ a [ href ("/doc/" ++ note.id) ] [ text note.title ] ]


fetchNotes : (Result Http.Error (List Document) -> msg) -> Cmd msg
fetchNotes msg =
    Http.get
        (baseUrl ++ "/latest?type=note")
        documentListResponseDecoder
        |> Http.send msg


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string


fetchDoc : String -> (Result Http.Error Document -> msg) -> Cmd msg
fetchDoc docId msg =
    Http.get
        (baseUrl ++ "/documents/" ++ docId)
        documentResponseDecoder
        |> Http.send msg



-- Document View


initialDocument : Document
initialDocument =
    { id = ""
    , title = "Loading..."
    , html = ""
    }


docView : Document -> String -> Html msg
docView document docId =
    div []
        [ h1 [ class "document-title" ] [ text document.title ]
        , div [ class "document-body" ]
            (HtmlParser.parse document.html
                |> HtmlParser.Util.toVirtualDom
            )
        ]



-- Decoders


type alias Document =
    { id : String
    , title : String
    , html : String
    }


type alias DocumentListResult =
    List Document


documentResponseDecoder : Decoder Document
documentResponseDecoder =
    Json.Decode.at [ "_embedded" ] documentDecoder


documentDecoder : Decoder Document
documentDecoder =
    decode Document
        |> required "id" Json.Decode.string
        |> required "title" Json.Decode.string
        |> required "html" Json.Decode.string


documentListResponseDecoder : Decoder (List Document)
documentListResponseDecoder =
    Json.Decode.at [ "_embedded" ] documentListDecoder


documentListDecoder : Decoder (List Document)
documentListDecoder =
    Json.Decode.list documentDecoder
