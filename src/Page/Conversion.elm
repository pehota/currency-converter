module Page.Conversion exposing (Model, Msg(..), decoder, init, update, view)

import Api
import Data.NonemptyList as NonemptyList exposing (NonemptyList)
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes as Attr
import Http
import Json.Decode as Decode
import Ports
import Session exposing (Session)


type Msg
    = RatesLoaded (Result Http.Error Currencies)


type Model
    = Loading
    | Error Http.Error
    | Success (ConversionField Source) (ConversionField Target)


type alias Currencies =
    NonemptyList CurrencyValue


type Source
    = Source


type Target
    = Target


type ConversionField a
    = ConversionField Currencies CurrencyValueInput


type CurrencyValueInput
    = CurrencyValueInput String (Maybe Float)


type CurrencyValue
    = CurrencyValue String Float


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading
    , Session.getSettings session |> .apiBaseUrl |> loadRates
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RatesLoaded (Ok rates) ->
            ( Success (initConversionField rates) (initConversionField rates)
            , Cmd.none
            )

        RatesLoaded (Err err) ->
            ( Error err
            , Ports.logError <| "Could not load conversion rates. Error: " ++ httpErrorToString err
            )


initConversionField : Currencies -> ConversionField a
initConversionField currencies =
    ConversionField currencies (CurrencyValueInput "1" (Just 1))


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl msg ->
            msg

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "There was a network error"

        Http.BadStatus statusCode ->
            "Server returnded " ++ String.fromInt statusCode

        Http.BadBody msg ->
            msg



-- View


view : Model -> Html Msg
view model =
    case model of
        Error err ->
            div [] [ text <| "Could not load conversion rates" ++ httpErrorToString err ]

        Loading ->
            div [] [ text "Loading ..." ]

        Success sourceCurrency targetCurrency ->
            renderForm sourceCurrency targetCurrency


renderForm : ConversionField Source -> ConversionField Target -> Html Msg
renderForm sourceCurrency targetCurrency =
    div []
        [ renderConversionField sourceCurrency
        , renderConversionField targetCurrency
        ]


getInputValue : CurrencyValueInput -> String
getInputValue (CurrencyValueInput userInput _) =
    userInput


renderConversionField : ConversionField a -> Html Msg
renderConversionField (ConversionField currencies value) =
    div []
        [ renderCurrencySelector currencies
        , input [ Attr.type_ "number", Attr.value <| getInputValue value ] []
        ]


renderCurrencySelector : Currencies -> Html Msg
renderCurrencySelector currencies =
    div [] [ text "Currencies selector" ]



-- Api


loadRates : String -> Cmd Msg
loadRates baseApiUrl =
    Api.getWithAuth (baseApiUrl ++ "/currencies") RatesLoaded decoder



-- Decoders


decoder : Decode.Decoder Currencies
decoder =
    Decode.field "rates" (Decode.list rateDecoder)
        |> Decode.andThen
            (NonemptyList.fromList
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty rates list")
            )


rateDecoder : Decode.Decoder CurrencyValue
rateDecoder =
    Decode.map2 CurrencyValue
        (Decode.field "sign" Decode.string)
        (Decode.field "rate" Decode.float)
