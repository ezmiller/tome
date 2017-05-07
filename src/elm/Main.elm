module Main exposing (..)

import Css exposing (pct, px)
import Date exposing (Date, fromString)
import Html
    exposing
        ( Html
        , Attribute
        , a
        , article
        , button
        , text
        , div
        , h1
        , h2
        , header
        , nav
        , h1
        , input
        , ul
        , li
        , span
        , textarea
        , time
        )
import Html.Attributes exposing (class, href, placeholder, value)
import HtmlParser
import HtmlParser.Util
import Http
import Json.Decode exposing (Decoder, at, string)
import Json.Decode.Pipeline exposing (..)
import Navbar exposing (navbar, MenuItem, navItem)
import Navigation exposing (Location)
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..), parseLocation)


baseUrl : String
baseUrl =
    "http://localhost:6789"


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { route : Route
    , document : WebData Document
    , notes : List Document
    }


type Msg
    = FetchDocument
    | DocumentFetched (WebData Document)
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

        DocumentFetched response ->
            ( { model | document = response }, Cmd.none )

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
            fetchDoc id

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
    ul [ class "post-list" ]
        (List.sortWith
            descendingUpdatedDates
            model.notes
            |> List.map renderNoteLink
        )


descendingUpdatedDates : { a | updated : Date } -> { b | updated : Date } -> Order
descendingUpdatedDates a b =
    case (Date.toTime a.updated) < (Date.toTime b.updated) of
        True ->
            GT

        False ->
            LT


renderNoteLink : Document -> Html msg
renderNoteLink note =
    li []
        [ article []
            [ h2 [] [ a [ href ("/doc/" ++ note.id) ] [ text note.title ] ]
            , span [ class "post-meta" ]
                [ time [] [ text (formatDate note.created) ]
                ]
            ]
        ]


formatDate : Date -> String
formatDate date =
    (toString (Date.month date))
        ++ " "
        ++ (toString (Date.day date))
        ++ ", "
        ++ (toString (Date.year date))


fetchNotes : (Result Http.Error (List Document) -> msg) -> Cmd msg
fetchNotes msg =
    Http.get
        (baseUrl ++ "/latest?type=note")
        documentListResponseDecoder
        |> Http.send msg


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string


fetchDoc : String -> Cmd Msg
fetchDoc docId =
    Http.get (baseUrl ++ "/documents/" ++ docId) documentResponseDecoder
        |> RemoteData.sendRequest
        |> Cmd.map DocumentFetched



-- Document View


initialDocument : WebData Document
initialDocument =
    Loading


docView : WebData Document -> String -> Html msg
docView document docId =
    case document of
        NotAsked ->
            text "Not Asked!"

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text "Oops! Something went wrong." ]

        Success document ->
            div []
                [ div [ class "document-body" ]
                    (HtmlParser.parse document.html
                        |> HtmlParser.Util.toVirtualDom
                    )
                ]



-- Decoders


type alias Document =
    { id : String
    , title : String
    , html : String
    , created : Date
    , updated : Date
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
        |> required "created-at" dateDecoder
        |> required "updated-at" dateDecoder


dateDecoder : Decoder Date
dateDecoder =
    Json.Decode.andThen
        (\string ->
            case Date.fromString string of
                Ok date ->
                    Json.Decode.succeed <| date

                Err err ->
                    Json.Decode.fail err
        )
        Json.Decode.string


documentListResponseDecoder : Decoder (List Document)
documentListResponseDecoder =
    Json.Decode.at [ "_embedded" ] documentListDecoder


documentListDecoder : Decoder (List Document)
documentListDecoder =
    Json.Decode.list documentDecoder
