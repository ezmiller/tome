module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Home
    | Editor
    | DocumentViewRoute String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Editor (s "editor")
        , map DocumentViewRoute (s "doc" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
