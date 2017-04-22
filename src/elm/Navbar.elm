module Navbar exposing (MenuItem, navItem, navbar)

import Html exposing (Html, Attribute, a, div, li, nav, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode


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


navItem : (String -> msg) -> String -> String -> Bool -> MenuItem msg
navItem newUrlMsg url label selected =
    itemLink
        [ href url
        , onWithOptions "click"
            { stopPropagation = True, preventDefault = True }
            (Json.Decode.succeed (newUrlMsg url))
        ]
        [ text label ]


navbar : List (MenuItem msg) -> Html msg
navbar menuItems =
    nav []
        [ div [] [ renderItems menuItems ] ]


renderItem : MenuItem msg -> Html msg
renderItem (MenuItem { attributes, children }) =
    li [ class "nav-item" ]
        [ a ([ class "nav-link" ] ++ attributes) children ]


renderItems : List (MenuItem msg) -> Html msg
renderItems items =
    ul [] (List.map renderItem items)
