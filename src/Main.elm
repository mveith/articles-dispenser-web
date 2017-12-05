module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, class, href, target, id)
import Http
import Json.Decode as Json
import Navigation
import Date
import Date.Format

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
        DownloadedArticles (Err e) -> ( model, Cmd.none)


---- VIEW ----

view : Model -> Html Msg
view model =
    let content =        
        case model.loginData of
        Just loginData -> 
            div [] 
            [
                Html.nav [class "navbar navbar-expand-lg navbar-light fixed-top navbar-shrink", id "mainNav"]
                [
                    div [class "container"]
                    [
                        Html.a [class "navbar-brand js-scroll-trigger", href "#page-top"] [text "ARTICLES DISPENSER"],
                        Html.ul [class "navbar-nav ml-auto"]
                        [
                            Html.li [class "nav-item"][ Html.a [class "nav-link js-scroll-trigger"][ text loginData.userName]]
                        ]
                    ]
                ],
                Html.section [class "articles h-100"]
                [                    
                    Html.a [ onClick DownloadArticles, class "btn btn-outline-primary btn-lg", Html.Attributes.attribute "role" "button", Html.Attributes.attribute "aria-pressed" "true" ][ text "Download articles"],
                    Html.br[][],
                    Html.br[][],
                    div [class "list-group"] (List.indexedMap articleRow (List.reverse model.articles))
                ]
            ]
        Nothing -> 
            Html.header [class "masthead h-100"] 
            [
                div [class "container h-100"]
                [
                    div [class "row h-100"] 
                    [
                        div [class "col-lg-12 my-auto"] 
                        [
                            div [class "header-content"]
                            [
                                Html.h1 [] [ Html.text "ARTICLES DISPENSER"],
                                Html.h4 [class "mb-5"] [ text "Pocket client for those who want to efficiently handle a large number of articles."],
                                Html.a [onClick Login, class "btn-rounded btn-outline btn-xl"] [text "Login and start"]
                            ]
                        ]
                    ]
                ]
            ]
    in
     div [class "h-100"] 
     [
      content,
      footerView model
     ]


footerView : Model -> Html Msg
footerView model =
    Html.footer []
    [
        Html.div [ class "container"] 
        [
            Html.p [] 
            [
                Html.text "©2017 Articles Dispenser. All Rights Reserved.",
                Html.ul [ class "list-inline"] 
                [
                    Html.li [class "list-inline-item"]
                    [
                        Html.a [ href "https://twitter.com/miroveith", target "_blank" ]
                        [
                            Html.i [class "fa fa-twitter"][]
                        ]
                    ],
                    Html.li [class "list-inline-item"]
                    [
                        Html.a [ href "https://github.com/mveith/articles-dispenser-web", target "_blank" ]
                        [
                            Html.i [class "fa fa-github"][]
                        ]
                    ]
                ]
            ]
        ]
    ]

articleRow: Int -> Article -> Html Msg
articleRow index article=
    Html.a [class "list-group-item list-group-item-action flex-column align-items-start"] 
    [
        Html.h5 [class "mb-1 word-wrap article-title"][text article.title],
        Html.p [class "text-left"] [Html.small [] [Html.strong [] [text "Tags: "], (text (String.join ", " article.tags))]],
        Html.span [ class "float-left"] 
        [
            Html.a [href article.url, target "_blank"] [Html.i [class "fa fa-link"][] ],
            text " ",
            Html.a [href ("https://getpocket.com/a/read/" ++ article.id), target "_blank"] [Html.i [class "fa fa-get-pocket"][] ]
        ],
        Html.span [ class "float-right"] 
        [
            Html.small [] [(text (dateView article.added))]
        ]
    ]

dateView : Maybe Date.Date -> String
dateView date =
    case date of
    Just d -> Date.Format.format "%d/%m/%Y" d
    Nothing -> ""

-- HTTP

apiUrl : String
apiUrl = "https://2gf3hu5hsh.execute-api.us-east-1.amazonaws.com/dev"

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