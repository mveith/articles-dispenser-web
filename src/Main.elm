module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json
import Navigation
import Date

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
         Just token -> ( Model Nothing location.origin [], getAccessToken token)
         Nothing -> ( Model Nothing location.origin [], Cmd.none )
    _-> ( Model Nothing location.origin [], Cmd.none )

---- MODEL ----

type alias Model =
    {
        loginData : Maybe LoginData,
        url : String,
        articles : List Article
    }

type alias LoginData =
    {
        userName : String,
        accessToken : String
    }

type alias Article =
    {
        url : String,
        id: String,
        title: String,
        excerpt: String,
        tags: List String,
        added: Maybe Date.Date,
        length: Maybe Int
    }


---- UPDATE ----

type Msg
    = Login
    | NewRequestToken (Result Http.Error String)
    | UrlChange Navigation.Location
    | LoggedIn (Result Http.Error LoginData)
    | DownloadArticles
    | DownloadedArticles (Result Http.Error (List Article))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login -> ( model, getRequestToken )
        NewRequestToken (Ok newRequestToken) -> ( model, navigateToLoginPage newRequestToken model.url)
        NewRequestToken (Err _) -> ( model, Cmd.none)
        UrlChange location -> ( model, Cmd.none)
        LoggedIn (Ok login) -> ( {model | loginData = Just login} , Navigation.modifyUrl "/")
        LoggedIn (Err _) -> ( model, Cmd.none)
        DownloadArticles -> 
            case model.loginData of
            Just data -> ( model, downloadArticles data.accessToken)
            Nothing -> ( model, Cmd.none)
        DownloadedArticles (Ok articles) -> ({ model | articles = articles}, Cmd.none)
        DownloadedArticles (Err e) -> 
            Debug.log (toString e)
            ( model, Cmd.none)


---- VIEW ----

view : Model -> Html Msg
view model =
    case model.loginData of
    Just loginData -> 
        div [] 
        [
            Html.span [] [ Html.text ("User: " ++ loginData.userName) ],
            Html.br [][],
            Html.button [ onClick DownloadArticles ] [text "Download articles"] ,
            Html.br [][],
            Html.ul [] (List.map articleView model.articles)
        ]
    Nothing -> 
        div []
        [
            Html.button [ onClick Login ] [text "Login"] 
        ]

articleView: Article -> Html Msg
articleView article=
    Html.li [][Html.text article.title]

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

downloadArticles: String -> Cmd Msg
downloadArticles accessToken =
    let 
     url =  apiUrl ++ "/getArticles?accesstoken=" ++ accessToken
    in
     Http.send DownloadedArticles (Http.get url decodeArticles)


decodeArticles : Json.Decoder (List Article)
decodeArticles = 
    Json.list 
        (Json.map7 Article 
            (Json.field "Url" Json.string)
            (Json.field "Id" Json.string)
            (Json.field "Title" Json.string)
            (Json.field "Excerpt" Json.string)
            (Json.field "Tags" (Json.list Json.string))
            (Json.map decodeDate (Json.field "Added" Json.string))
            (Json.map decodeInt (Json.maybe (Json.field "WordCount" Json.string))))

decodeDate : String -> Maybe Date.Date
decodeDate date = 
    let result = Date.fromString date
    in
     case result of
     Ok date -> Just date
     Err _ -> Nothing

decodeInt : Maybe String -> Maybe Int
decodeInt value = 
    case value of
    Just v -> 
        case String.toInt v of
        Ok i -> Just i
        Err _ -> Nothing
    Nothing -> Nothing