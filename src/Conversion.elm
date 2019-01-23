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
    | CurrencyChanged FieldPosition String
    | ValueChanged FieldPosition String


type Model
    = Loading (Maybe UrlParam) (Maybe UrlParam)
    | Error Http.Error
    | Success ConversionField ConversionField


type alias Currencies =
    SelectList CurrencyValue


type FieldPosition
    = Source
    | Target


type alias ConversionField =
    { currencies : Currencies
    , value : CurrencyValueInput
    , position : FieldPosition
    }


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


initConversionField : FieldPosition -> Currencies -> ConversionField
initConversionField position currencies =
    { currencies = currencies
    , value = CurrencyValueInput "1" (Just 1)
    , position = position
    }


initConversionFieldFromParam : UrlParam -> FieldPosition -> Currencies -> ConversionField
initConversionFieldFromParam (UrlParam sign value) position currencies =
    initConversionField position currencies
        |> updateFieldCurrency sign
        |> updateFieldValue (String.fromFloat value)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RatesLoaded (Ok rates), Loading Nothing Nothing ) ->
            ( Success (initConversionField Source rates) (initConversionField Target rates)
            , Cmd.none
            )

        ( RatesLoaded (Ok rates), Loading (Just source) (Just target) ) ->
            let
                sourceField =
                    initConversionFieldFromParam source Source rates

                targetField =
                    convert sourceField
                        (initConversionFieldFromParam target Target rates)
            in
            ( Success sourceField targetField
            , Cmd.none
            )

        ( RatesLoaded (Err err), _ ) ->
            ( Error err
            , Ports.logError <| "Could not load conversion rates. Error: " ++ httpErrorToString err
            )

        ( CurrencyChanged Source currencySign, Success sourceField targetField ) ->
            let
                updatedSource =
                    updateFieldCurrency currencySign sourceField
            in
            ( Success updatedSource (convert updatedSource targetField)
            , Cmd.none
            )

        ( CurrencyChanged Target currencySign, Success sourceField targetField ) ->
            let
                updatedTarget =
                    updateFieldCurrency currencySign targetField
            in
            ( Success (convert updatedTarget sourceField) updatedTarget
            , Cmd.none
            )

        ( ValueChanged Source value, Success sourceField targetField ) ->
            let
                updatedSource =
                    updateFieldValue value sourceField
            in
            ( Success updatedSource (convert updatedSource targetField)
            , Cmd.none
            )

        ( ValueChanged Target value, Success sourceField targetField ) ->
            let
                updatedTarget =
                    updateFieldValue value targetField
            in
            ( Success (convert updatedTarget sourceField) updatedTarget
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


toEuro : Float -> Float
toEuro currencyRate =
    1 / currencyRate


convert : ConversionField -> ConversionField -> ConversionField
convert source target =
    let
        -- Since the backend only returns a list of currencies and the co-responding currency/euro rates
        -- we first need to convert both currencies to the same base in euro
        ratio =
            (toEuro <| getFieldCurrencyRate source)
                / (toEuro <| getFieldCurrencyRate target)
    in
    case getFieldInputValue source of
        Just sourceAmount ->
            updateFieldValue (String.fromFloat (sourceAmount * ratio)) target

        _ ->
            target


updateFieldCurrency : String -> ConversionField -> ConversionField
updateFieldCurrency sign ({ currencies } as field) =
    { field | currencies = SelectList.select (.sign >> (==) sign) currencies }


clipNumericalString : String -> String
clipNumericalString numerical =
    case String.split "." numerical of
        [ wholeNum, decimals ] ->
            String.join "." [ wholeNum, String.left 2 decimals ]

        _ ->
            numerical


updateFieldValue : String -> ConversionField -> ConversionField
updateFieldValue userInput field =
    let
        clipped =
            clipNumericalString userInput
    in
    { field | value = CurrencyValueInput clipped (String.toFloat clipped) }


getFieldCurrencyRate : ConversionField -> Float
getFieldCurrencyRate =
    .currencies >> SelectList.selected >> .rate


getFieldInputValue : ConversionField -> Maybe Float
getFieldInputValue { value } =
    let
        (CurrencyValueInput _ val) =
            value
    in
    val



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


renderForm : ConversionField -> ConversionField -> Html Msg
renderForm sourceField targetField =
    div []
        [ renderSourceConversionField sourceField
        , renderTargetConversionField targetField
        ]


renderSourceConversionField : ConversionField -> Html Msg
renderSourceConversionField =
    renderConversionField (CurrencyChanged Source) (ValueChanged Source)


renderTargetConversionField : ConversionField -> Html Msg
renderTargetConversionField =
    renderConversionField (CurrencyChanged Target) (ValueChanged Target)


renderConversionField : (String -> Msg) -> (String -> Msg) -> ConversionField -> Html Msg
renderConversionField toCurrencyChangedMsg toValueChangedMsg { currencies, value, position } =
    div []
        [ renderCurrencySelector (CurrencyChanged position) currencies
        , renderCurrencyValueInput (ValueChanged position) value
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
            (List.sortBy .sign
                >> SelectList.fromList
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
