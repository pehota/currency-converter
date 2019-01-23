module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Conversion
import Flags
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onInput)
import Html.Helpers
import Json.Decode as Decode
import Json.Encode as Encode
import Route
import Session exposing (Session)
import Url exposing (Url)


type Msg
    = NoOp
    | UrlChanged Url
    | LinkClicked UrlRequest
    | CurrencyConversionMsg Conversion.Msg


type Model
    = Loading
    | Error String
    | NotFound
    | CurrencyConversion Conversion.Model


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    case Decode.decodeValue Flags.decoder flags of
        Ok config ->
            Session.create navKey config
                |> setRoute url

        Err err ->
            ( Error <| Decode.errorToString err, Cmd.none )


setRoute : Url -> Session -> ( Model, Cmd Msg )
setRoute url session =
    case Route.fromUrl url of
        Just Route.Root ->
            Conversion.init session
                |> updateWith CurrencyConversion CurrencyConversionMsg

        Just (Route.Convert sourceCurrencyParam targetCurrencyParam) ->
            Conversion.initWithParams session sourceCurrencyParam targetCurrencyParam
                |> updateWith CurrencyConversion CurrencyConversionMsg

        Nothing ->
            ( NotFound, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


view : Model -> Document Msg
view model =
    { title = " Currency Converter "
    , body = renderPage model
    }


renderPage : Model -> List (Html Msg)
renderPage model =
    List.singleton <|
        case model of
            Loading ->
                div [ Attr.class "app-loading-spinner" ] []

            NotFound ->
                div [] [ text "Not Found" ]

            Error error ->
                Html.Helpers.errorPage error

            CurrencyConversion conversionData ->
                Html.map CurrencyConversionMsg <| Conversion.view conversionData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( CurrencyConversionMsg subMsg, CurrencyConversion subModel ) ->
            Conversion.update subMsg subModel
                |> updateWith CurrencyConversion CurrencyConversionMsg

        _ ->
            ( model, Cmd.none )
