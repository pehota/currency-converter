module Html.Helpers exposing
    ( errorPage
    , loadSpinner
    )

import Html exposing (Html, div, text)


errorPage : String -> Html msg
errorPage error =
    div [] [ text <| "Error: " ++ error ]


loadSpinner : Html msg
loadSpinner =
    div [] [ text " loading ... " ]
