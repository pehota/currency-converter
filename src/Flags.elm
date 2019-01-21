module Flags exposing (Flags, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Flags =
    { apiBaseUrl : String }


decoder : Decoder Flags
decoder =
    Decode.map Flags
        (Decode.field "apiBaseUrl" Decode.string)
