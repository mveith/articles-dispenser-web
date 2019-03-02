module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Json.Decode as Json
import Url
import Model exposing (Model, ModelDto, LoginData)
import Update exposing (update)
import Effects
import Messages exposing (Msg)
import View exposing (view)


main : Program Json.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = Messages.UrlChanged
    , onUrlRequest = Messages.LinkClicked
    }

init : Json.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case url.path of
    "/authorized"-> 
        let
         authorizedRequestToken = case url.query of 
            Just query -> List.head (List.reverse (String.split "=" query))
            Nothing -> Nothing
         location = Maybe.withDefault "" (List.head (String.split url.path (Url.toString url)))
        in
         case authorizedRequestToken of
         Just token -> ( Model Nothing location [] Nothing Nothing [] Nothing key, Effects.getAccessToken token)
         Nothing -> initDefault location key
    _ ->
        case Json.decodeValue decodeModelDto flags of
        Ok modelDto -> initFromCache modelDto url key
        Err e -> initDefault (String.dropRight 1 (Url.toString url)) key

initFromCache : ModelDto -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
initFromCache modelDto url key =
    case modelDto.loginData of 
    Just login -> ( Model (Just login) (String.dropRight 1 (Url.toString url)) [] Nothing modelDto.tagsFilter modelDto.allArticles modelDto.maxLengthFilter key, Cmd.batch [Effects.filter, Effects.downloadArticles login.accessToken] )
    Nothing -> initDefault (String.dropRight 1 (Url.toString url)) key

initDefault : String -> Nav.Key -> ( Model, Cmd Msg )
initDefault url key =
    ( Model Nothing url [] Nothing Nothing [] Nothing key, Cmd.none )

decodeModelDto : Json.Decoder (ModelDto)
decodeModelDto = 
    Json.map4 ModelDto 
        (Json.field "loginData" (Json.maybe (Json.map2 LoginData 
            (Json.field "userName" Json.string) 
            (Json.field "accessToken" Json.string))))
        (Json.field "tagsFilter" (Json.maybe Json.string))
        (Json.field "maxLengthFilter" (Json.maybe Json.int))
        (Json.field "allArticles" decodeArticles)

        
decodeArticles : Json.Decoder (List Model.Article)
decodeArticles = 
    Json.list 
        (Json.map7 Model.Article 
            (Json.field "url" Json.string)
            (Json.field "id" Json.string)
            (Json.field "title" Json.string)
            (Json.field "excerpt" Json.string)
            (Json.field "tags" (Json.list Json.string))
            (Json.field "added" (Json.maybe (Json.string)))
            (Json.field "length" (Json.maybe (Json.int))))