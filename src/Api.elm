module Api exposing (Token, getWithAuth)

import Http
import Json.Decode as JD
import Task
import Time


type ApiCall
    = LoadCurrencies String


type Token
    = Token String


tokenFromString : String -> Token
tokenFromString =
    Token


tokenToString : Token -> String
tokenToString (Token val) =
    val


getWithAuth : String -> (Result Http.Error a -> msg) -> JD.Decoder a -> Cmd msg
getWithAuth =
    requestWithAuth "GET"


requestWithAuth : String -> String -> (Result Http.Error a -> msg) -> JD.Decoder a -> Cmd msg
requestWithAuth method url toMsg responseDecoder =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" "Token some-made-up-token" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
