module Html.Extra exposing (link)

import Html exposing (..)
import Html.Attributes exposing (..)


link : String -> List (Attribute msg) -> List (Html msg) -> Html msg
link target attributes children =
    a [ href target ] children
