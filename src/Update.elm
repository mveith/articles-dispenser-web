module Update exposing (..)

import Http
import Navigation
import Random
import Model exposing (Model, LoginData, Article)
import Messages exposing (..)
import Effects exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login -> 
            ( model, getRequestToken )
        NewRequestToken (Ok newRequestToken) -> 
            ( model, navigateToLoginPage newRequestToken model.url)
        NewRequestToken (Err _) -> 
            ( model, Cmd.none)
        UrlChange location -> 
            ( model, Cmd.none)
        LoggedIn (Ok login) -> 
            ( {model | loginData = Just login} , Navigation.modifyUrl "/")
        LoggedIn (Err _) -> 
            ( model, Cmd.none)
        DownloadArticles -> 
            case model.loginData of
            Just data -> ( model, downloadArticles data.accessToken)
            Nothing -> ( model, Cmd.none)
        DownloadedArticles (Ok articles) -> 
            ({ model | articles = articles}, randomizeArticles articles)
        DownloadedArticles (Err e) -> 
            ( model, Cmd.none)
        GenerateRandomArticle -> 
            (model, randomizeArticles model.articles)
        RandomizedArticles a -> 
            ( { model | randomArticle = (List.head a)}, Cmd.none)
