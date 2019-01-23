module Conversion exposing
    ( Model
    , Msg(..)
    , UrlParam
    , decoder
    , init
    , initWithParams
    , parser
    , update
    , urlParamToString
    , view
    )

import Api
import Data.SelectList as SelectList exposing (SelectList)
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes as Attr
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Ports
import Session exposing (Session)
import Url.Parser as Parser exposing ((</>), Parser)


type Msg
    = NoOp
    | RatesLoaded (Result Http.Error Currencies)
    | SourceCurrencyChanged String
    | TargetCurrencyChanged String
    | SourceValueChanged String
    | TargetValueChanged String


type Model
    = Loading (Maybe UrlParam) (Maybe UrlParam)
    | Error Http.Error
    | Success (ConversionField Source) (ConversionField Target)


type alias Currencies =
    SelectList CurrencyValue


type Source
    = Source


type Target
    = Target


type ConversionField a
    = ConversionField Currencies CurrencyValueInput


type CurrencyValueInput
    = CurrencyValueInput String (Maybe Float)


type alias CurrencyValue =
    { sign : String, rate : Float }


type UrlParam
    = UrlParam String Float


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading Nothing Nothing
    , Session.getSettings session |> .apiBaseUrl |> loadRates
    )


initWithParams : Session -> UrlParam -> UrlParam -> ( Model, Cmd Msg )
initWithParams session source target =
    ( Loading (Just source) (Just target)
    , Session.getSettings session |> .apiBaseUrl |> loadRates
    )


initConversionField : Currencies -> ConversionField a
initConversionField currencies =
    ConversionField currencies (CurrencyValueInput "1" (Just 1))


initConversionFieldFromParam : Currencies -> UrlParam -> ConversionField a
initConversionFieldFromParam currencies (UrlParam sign value) =
    initConversionField currencies
        |> updateFieldCurrency sign
        |> updateFieldValue (String.fromFloat value)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RatesLoaded (Ok rates), Loading Nothing Nothing ) ->
            ( Success (initConversionField rates) (initConversionField rates)
            , Cmd.none
            )

        ( RatesLoaded (Ok rates), Loading (Just source) (Just target) ) ->
            ( Success
                (initConversionFieldFromParam rates source)
                (initConversionFieldFromParam rates target)
            , Cmd.none
            )

        ( RatesLoaded (Err err), _ ) ->
            ( Error err
            , Ports.logError <| "Could not load conversion rates. Error: " ++ httpErrorToString err
            )

        ( SourceCurrencyChanged currencySign, Success sourceField targetField ) ->
            ( Success (updateFieldCurrency currencySign sourceField) targetField
            , Cmd.none
            )

        ( TargetCurrencyChanged currencySign, Success sourceField targetField ) ->
            ( Success sourceField (updateFieldCurrency currencySign targetField)
            , Cmd.none
            )

        ( SourceValueChanged value, Success sourceField targetField ) ->
            ( Success (updateFieldValue value sourceField) targetField
            , Cmd.none
            )

        ( TargetValueChanged value, Success sourceField targetField ) ->
            ( Success sourceField (updateFieldValue value targetField)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateFieldCurrency : String -> ConversionField a -> ConversionField a
updateFieldCurrency sign (ConversionField currencies value) =
    ConversionField (SelectList.select (.sign >> (==) sign) currencies) value


clipNumericalString : String -> String
clipNumericalString numerical =
    case String.split "." numerical of
        [ wholeNum, decimals ] ->
            String.join "." [ wholeNum, String.left 2 decimals ]

        _ ->
            numerical


updateFieldValue : String -> ConversionField a -> ConversionField a
updateFieldValue userInput (ConversionField currencies value) =
    let
        clipped =
            clipNumericalString userInput
    in
    ConversionField currencies <|
        CurrencyValueInput clipped (String.toFloat clipped)



-- View


view : Model -> Html Msg
view model =
    case model of
        Error err ->
            div [] [ text <| "Could not load conversion rates" ++ httpErrorToString err ]

        Loading _ _ ->
            div [] [ text "Loading ..." ]

        Success sourceField targetField ->
            renderForm sourceField targetField


renderForm : ConversionField Source -> ConversionField Target -> Html Msg
renderForm sourceField targetField =
    div []
        [ renderSourceConversionField sourceField
        , renderTargetConversionField targetField
        ]


renderSourceConversionField : ConversionField Source -> Html Msg
renderSourceConversionField =
    renderConversionField SourceCurrencyChanged SourceValueChanged


renderTargetConversionField : ConversionField Target -> Html Msg
renderTargetConversionField =
    renderConversionField TargetCurrencyChanged TargetValueChanged


renderConversionField : (String -> Msg) -> (String -> Msg) -> ConversionField a -> Html Msg
renderConversionField toCurrencyChangedMsg toValueChangedMsg (ConversionField currencies value) =
    div []
        [ renderCurrencySelector toCurrencyChangedMsg currencies
        , renderCurrencyValueInput toValueChangedMsg value
        ]


getCurrencySign : CurrencyValue -> String
getCurrencySign { sign } =
    sign


getInputValue : CurrencyValueInput -> String
getInputValue (CurrencyValueInput userInput _) =
    userInput


renderCurrencyValueInput : (String -> Msg) -> CurrencyValueInput -> Html Msg
renderCurrencyValueInput toMsg value =
    input [ Attr.type_ "number", Attr.value <| getInputValue value, onInput toMsg ] []


renderCurrencySelector : (String -> Msg) -> Currencies -> Html Msg
renderCurrencySelector toMsg currencies =
    let
        selectedCurrency =
            SelectList.selected currencies
    in
    div []
        [ select [ onInput toMsg ] <|
            List.map
                (\currency -> renderOption (getCurrencySign currency) (currency == selectedCurrency))
                (SelectList.toList currencies)
        ]


renderOption : String -> Bool -> Html Msg
renderOption value isSelected =
    option [ Attr.value value, Attr.selected isSelected ] [ text value ]



-- Helpers


loadRates : String -> Cmd Msg
loadRates baseApiUrl =
    Api.getWithAuth (baseApiUrl ++ "/currencies") RatesLoaded decoder


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



-- Decoders


decoder : Decode.Decoder Currencies
decoder =
    Decode.field "rates" (Decode.list rateDecoder)
        |> Decode.andThen
            (SelectList.fromList
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty rates list")
            )


rateDecoder : Decode.Decoder CurrencyValue
rateDecoder =
    Decode.map2 CurrencyValue
        (Decode.field "sign" Decode.string)
        (Decode.field "rate" Decode.float)



-- Route parser


parser : Parser (UrlParam -> UrlParam -> a) a
parser =
    paramParser </> paramParser


paramParser : Parser (UrlParam -> a) a
paramParser =
    Parser.custom "CONVERSION_URL_PARAM" urlParamFromString


urlParamFromString : String -> Maybe UrlParam
urlParamFromString str =
    case String.split ":" str of
        [ currencySign, currencyValue ] ->
            Maybe.map
                (UrlParam (String.toUpper currencySign))
                (String.toFloat currencyValue)

        _ ->
            Nothing


urlParamToString : UrlParam -> String
urlParamToString (UrlParam sign value) =
    String.join ":" [ sign, String.fromFloat value ]
