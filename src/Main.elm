module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json
import Task exposing (..)
import Navigation

---- PROGRAM ----

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

init :Navigation.Location -> ( Model, Cmd Msg )
init location =
    case location.pathname of
    "/authorized"-> 
        let
         authorizedRequestToken = List.head (List.reverse (String.split "=" location.search))
        in
         case authorizedRequestToken of
         Just token -> ( Model Nothing location.origin, getAccessToken token)
         Nothing -> ( Model Nothing location.origin, Cmd.none )
    _-> ( Model Nothing location.origin, Cmd.none )

---- MODEL ----

type alias Model =
    {
        loginData : Maybe LoginData,
        url : String
    }

type alias LoginData =
    {
        userName : String,
        accessToken : String
    }


---- UPDATE ----

type Msg
    = Login
    | NewRequestToken (Result Http.Error String)
    | UrlChange Navigation.Location
    | LoggedIn (Result Http.Error LoginData)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login -> ( model, getRequestToken )
        NewRequestToken (Ok newRequestToken) -> ( model, navigateToLoginPage newRequestToken model.url)
        NewRequestToken (Err _) -> ( model, Cmd.none)
        UrlChange location -> ( model, Cmd.none)
        LoggedIn (Ok login) -> ( {model | loginData = Just login} , Navigation.modifyUrl "/")
        LoggedIn (Err _) -> ( model, Cmd.none)


---- VIEW ----

view : Model -> Html Msg
view model =
    case model.loginData of
    Just loginData -> 
        div [] 
        [
            Html.span [] [ Html.text ("User: " ++ loginData.userName) ]
        ]
    Nothing -> 
        div []
        [
            Html.button [ onClick Login ] [text "Login"] 
        ]

-- HTTP

apiUrl : String
apiUrl = "http://localhost:3000"

getRequestToken : Cmd Msg
getRequestToken=
    let 
     url = apiUrl ++  "/getRequestToken"
    in
     Http.send NewRequestToken (Http.get url decodeRequestToken)

decodeRequestToken : Json.Decoder String
decodeRequestToken =
    Json.at ["requestToken"] Json.string

navigateToLoginPage: String -> String -> Cmd Msg
navigateToLoginPage requestToken actualUrl=
    let
     baseUrl = "https://getpocket.com/auth/authorize?request_token="
     redirectUrl = actualUrl ++ "/authorized?requestToken=" ++ requestToken
     url = baseUrl ++ requestToken ++ "&redirect_uri=" ++ redirectUrl
    in
        
    Navigation.load url

getAccessToken: String -> Cmd Msg
getAccessToken requestToken =
    let 
     url =  apiUrl ++ "/getAccessToken?key=" ++ requestToken
    in
     Http.send LoggedIn (Http.get url decodeAccessToken)

decodeAccessToken : Json.Decoder LoginData
decodeAccessToken =
    Json.map2 LoginData (Json.field "userName" Json.string) (Json.field "accessToken" Json.string)
