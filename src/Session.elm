module Session exposing (Session, create, getSettings)

import Api
import Browser.Navigation as Nav
import Flags exposing (Flags)


type Session
    = Session SessionData


type alias SessionData =
    { key : Nav.Key
    , token : Maybe Api.Token
    , apiBaseUrl : String
    }


create : Nav.Key -> Flags -> Session
create key { apiBaseUrl } =
    Session { key = key, token = Nothing, apiBaseUrl = apiBaseUrl }


getSettings : Session -> Flags
getSettings (Session data) =
    { apiBaseUrl = data.apiBaseUrl }
