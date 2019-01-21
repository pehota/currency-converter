module Page.Conversion exposing (Model, Msg(..), decoder, init, update, view)

import Api
import Data.NonemptyList as NonemptyList exposing (NonemptyList)
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode
import Ports
import RemoteData exposing (WebData)
import Session exposing (Session)


type alias Model =
    { rates : WebData Rates
    }


type Msg
    = RatesLoaded (Result Http.Error Rates)


type CurrencyRate
    = CurrencyRate String Float


type alias Rates =
    NonemptyList CurrencyRate


init : Session -> ( Model, Cmd Msg )
init session =
    ( { rates = RemoteData.Loading }
    , Session.getSettings session |> .apiBaseUrl |> loadRates
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RatesLoaded (Ok rates) ->
            ( { model | rates = RemoteData.Success rates }, Cmd.none )

        RatesLoaded (Err err) ->
            ( { model | rates = RemoteData.Failure err }
            , Ports.logError <| "Could not load conversion rates. Error: " ++ httpErrorToString err
            )


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


view : Model -> Html Msg
view { rates } =
    case rates of
        RemoteData.Failure err ->
            div [] [ text <| "Error: Could not load conversion rates" ]

        RemoteData.Loading ->
            div [] [ text "Loading ..." ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Success conversionRates ->
            renderForm conversionRates


renderForm : Rates -> Html Msg
renderForm rates =
    div [] [ text "show conversion form" ]


loadRates : String -> Cmd Msg
loadRates baseApiUrl =
    Api.getWithAuth (baseApiUrl ++ "/currencies") RatesLoaded decoder


decoder : Decode.Decoder Rates
decoder =
    Decode.field "rates" (Decode.list rateDecoder)
        |> Decode.andThen
            (NonemptyList.fromList
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty rates list")
            )


rateDecoder : Decode.Decoder CurrencyRate
rateDecoder =
    Decode.map2 CurrencyRate
        (Decode.field "sign" Decode.string)
        (Decode.field "rate" Decode.float)
