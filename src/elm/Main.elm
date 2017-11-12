module Main exposing (..)

import Css
    exposing
        ( backgroundColor
        , color
        , cursor
        , em
        , hex
        , fontVariant
        , fontWeight
        , fontSize
        , lighter
        , margin2
        , marginRight
        , padding2
        , pointer
        , px
        , smallCaps
        )
import Date exposing (Date, fromString)
import Html
    exposing
        ( Html
        , Attribute
        , a
        , br
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
import Html.Attributes exposing (attribute, class, href, placeholder, value)
import HtmlParser
import HtmlParser.Util
import Http
import Json.Decode exposing (Decoder, at, string)
import Json.Decode.Pipeline exposing (..)
import Navbar exposing (navbar, MenuItem, navItem)
import Navigation exposing (Location)
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..), parseLocation)
import String.Extra exposing (toSentenceCase)


-- For using styles in-line:
-- https://github.com/rtfeldman/elm-css#approach-1-inline-styles


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type alias Model =
    { apiRoot : String
    , route : Route
    , tagFilter : Maybe String
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

        initialFilter =
            Nothing

        model =
            Model flags.apiRoot initialRoute initialFilter initialDocument initialNotes

        cmd =
            getRouteCmd model initialRoute
    in
        ( model
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
            getRouteCmd model newRoute
    in
        ( { model | route = newRoute }, cmd )


getRouteCmd : Model -> Route -> Cmd Msg
getRouteCmd model route =
    case route of
        Home tags ->
            fetchNotes model.apiRoot tags

        DocumentViewRoute id ->
            fetchDoc model.apiRoot id

        _ ->
            Cmd.none


menuItems : List (MenuItem Msg)
menuItems =
    [ navItem NewUrl "/" "Home" False ]


view : Model -> Html Msg
view model =
    let
        pageClass =
            if (model.route == (Home Nothing)) then
                "notebook-home"
            else
                "notebook-document"
    in
        div [ class "container" ]
            [ div
                [ class ("page ten columns " ++ pageClass)
                , attribute "itemscope" ""
                , attribute "itemtype" "http://schema.org/Article"
                ]
                [ pageHeader
                , renderCurrentRoute model
                ]
            ]


renderCurrentRoute : Model -> Html Msg
renderCurrentRoute model =
    case model.route of
        Home tags ->
            home model tags

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


home : Model -> Maybe String -> Html msg
home model tags =
    case model.notes of
        NotAsked ->
            text "Not Asked!"

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text "Oops! Something went wrong." ]

        Success notes ->
            div [ class "posts twelve columns" ]
                [ (renderNotesHeader tags)
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


renderNotesHeader : Maybe String -> Html msg
renderNotesHeader tags =
    let
        headerText =
            case tags of
                Nothing ->
                    "Notes"

                Just tags ->
                    ("Notes (for tag: " ++ tags ++ ")")
    in
        h2 [ class "posts-year" ] [ text headerText ]


renderNoteLink : Document -> Html msg
renderNoteLink note =
    li []
        [ article []
            [ span
                [ class "post-meta"
                , styles [ fontSize (px 14) ]
                ]
                [ time [] [ text (formatDate note.created) ] ]
            , h2
                [ styles [ margin2 (px 0) (px 0) ] ]
                [ a [ href ("/notebook/doc/" ++ note.id) ] [ text note.title ] ]
            , (renderTags note.tags)
            ]
        ]


renderTags : List String -> Html msg
renderTags tags =
    span
        [ class "tag-list" ]
        [ span [ class "tag-list__tags" ] (List.map renderTag tags) ]


renderTag : String -> Html msg
renderTag tag =
    a
        [ class "tag-list__tag"
        , href ("/notebook?tags=" ++ tag)
        , styles
            [ marginRight (px 4)
            , padding2 (px 4) (px 6)
            , backgroundColor (hex "#B9B1A8")
            , fontSize (px 14)
            , cursor pointer
            , color (hex "#FFFFFF")
            ]
        ]
        [ text (toSentenceCase tag) ]


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


fetchNotes : String -> Maybe String -> Cmd Msg
fetchNotes apiRoot tagFilter =
    let
        url =
            case tagFilter of
                Nothing ->
                    (apiRoot ++ "/latest?type=note")

                Just tag ->
                    (apiRoot ++ "/latest?type=note&tags=" ++ tag)
    in
        Http.get
            url
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
                [ article
                    [ class "document-body"
                    , attribute "itemtype" "http://schema.org/articleBody"
                    ]
                    (HtmlParser.parse document.html
                        |> HtmlParser.Util.toVirtualDom
                    )
                ]



-- Decoders


type alias Document =
    { id : String
    , title : String
    , tags : List String
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
        |> required "tags" (Json.Decode.list Json.Decode.string)
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
