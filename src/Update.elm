port module Update exposing (..)

import Http
import Navigation
import Random
import Model exposing (Model, LoginData, Article)
import Messages exposing (..)
import Effects exposing (..)

port saveLoginData : LoginData -> Cmd msg

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
            ( {model | loginData = Just login} , Cmd.batch[ Navigation.modifyUrl "/", saveLoginData login, downloadArticles login.accessToken ])
        LoggedIn (Err _) -> 
            ( model, Cmd.none)
        DownloadedArticles (Ok articles) -> 
            ({ model | articles = sort articles, allArticles = articles}, randomizeArticles articles)
        DownloadedArticles (Err e) -> 
            ( model, Cmd.none)
        GenerateRandomArticle -> 
            (model, randomizeArticles model.articles)
        RandomizedArticles a -> 
            ( { model | randomArticle = (List.head a)}, Cmd.none)
        TagsFilter value -> ({model | tagsFilter = Just value } , Cmd.none)
        Filter -> 
            case model.tagsFilter of
            Just f -> ({model| articles = sort (filterArticles f model.allArticles)}, Cmd.none)
            Nothing -> ({model| articles = sort model.allArticles}, Cmd.none)
