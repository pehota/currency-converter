module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Conversion
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Root
    | Convert Conversion.UrlParam Conversion.UrlParam


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map Convert Conversion.parser
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Root ->
                    []

                Convert sourceCurrencyParam targetCurrencyParam ->
                    List.map
                        Conversion.urlParamToString
                        [ sourceCurrencyParam, targetCurrencyParam ]
    in
    "#/" ++ String.join "/" pieces
