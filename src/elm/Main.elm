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
        , p
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


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { apiRoot : String
    , route : Route
    , document : WebData Document
    , notes : WebData (List Document)
    }


type Msg
    = FetchDocument
    | DocumentFetched (WebData Document)
    | NotesFetched (WebData (List Document))
    | UrlChange Navigation.Location
    | NewUrl String


type alias Flags =
    { apiRoot : String
    }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialRoute =
            parseLocation location

        cmd =
            getRouteCmd flags.apiRoot initialRoute
    in
        ( Model flags.apiRoot initialRoute initialDocument initialNotes
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

        NotesFetched response ->
            ( { model | notes = response }, Cmd.none )

        UrlChange location ->
            changeLocation location model

        NewUrl url ->
            ( model, Navigation.newUrl url )


changeLocation : Location -> Model -> ( Model, Cmd Msg )
changeLocation location model =
    let
        newRoute =
            Debug.log "newRoute: " parseLocation location

        cmd =
            getRouteCmd model.apiRoot newRoute
    in
        ( { model | route = newRoute }, cmd )


getRouteCmd : String -> Route -> Cmd Msg
getRouteCmd apiRoot route =
    case route of
        Home ->
            fetchNotes apiRoot

        DocumentViewRoute id ->
            fetchDoc apiRoot id

        _ ->
            Cmd.none


menuItems : List (MenuItem Msg)
menuItems =
    [ navItem NewUrl "/" "Home" False ]


view : Model -> Html Msg
view model =
    let
        pageClass =
            if (model.route == Home) then
                "notebook-home"
            else
                "notebook-document"
    in
        div [ class "container" ]
            [ div
                [ class ("page ten columns " ++ pageClass) ]
                [ pageHeader
                , renderCurrentRoute model
                ]
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


headerIntroText : String
headerIntroText =
    "This is an open notebook containing rough notes on a range of topics that are of interest to me and which will form the basis for future writing. My hope is that they may be of use to others as well."


pageHeader : Html msg
pageHeader =
    header []
        [ h1 [ class "site-title" ]
            [ a
                [ class "site-title__link"
                , href "http://humanscode.com"
                ]
                [ text "HumansCode / Open Notebook" ]
            ]
        , p [] [ text headerIntroText ]
        ]



-- Home


initialNotes : WebData (List Document)
initialNotes =
    Loading


home : Model -> Html msg
home model =
    case model.notes of
        NotAsked ->
            text "Not Asked!"

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text "Oops! Something went wrong." ]

        Success notes ->
            div [ class "posts twelve columns" ]
                [ h2 [ class "posts-year" ] [ text "Notes" ]
                , ul [ class "post-list" ]
                    (List.sortWith
                        descendingUpdatedDates
                        notes
                        |> List.map renderNoteLink
                    )
                ]


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
            [ h2 [] [ a [ href ("/notebook/doc/" ++ note.id) ] [ text note.title ] ]
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
        ++ " at "
        ++ (toString (Date.hour date))
        ++ ":"
        ++ (toString (Date.minute date))


fetchNotes : String -> Cmd Msg
fetchNotes apiRoot =
    Http.get
        (apiRoot ++ "/latest?type=note")
        documentListResponseDecoder
        |> RemoteData.sendRequest
        |> Cmd.map NotesFetched


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    at [ "target", "value" ] string


fetchDoc : String -> String -> Cmd Msg
fetchDoc apiRoot docId =
    Http.get (apiRoot ++ "/documents/" ++ docId) documentResponseDecoder
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
                [ article [ class "document-body" ]
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
