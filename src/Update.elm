port module Update exposing (..)

import Navigation
import Model exposing (Model, LoginData, Article)
import Messages exposing (..)
import Effects exposing (..)
import Task

port saveData : Model -> Cmd msg
        
saveModel: Cmd Msg
saveModel = Task.succeed SaveModel |> Task.perform identity

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
            ( {model | loginData = Just login} , Cmd.batch[ Navigation.modifyUrl "/", saveModel, downloadArticles login.accessToken ])
        LoggedIn (Err _) -> 
            ( model, Cmd.none)
        DownloadedArticles (Ok articles) -> 
            ({ model | articles = sort articles, allArticles = articles}, Cmd.batch [Effects.filter, randomizeArticles articles, saveModel])
        DownloadedArticles (Err e) -> 
            ( model, Cmd.none)
        GenerateRandomArticle -> 
            (model, randomizeArticles model.articles)
        RandomizedArticles a -> 
            ( { model | randomArticle = (List.head a)}, Cmd.none)
        TagsFilter value -> ({model | tagsFilter = Just value } , Cmd.none)
        MaxLengthFilter value -> 
            let
                intValue = Result.withDefault -1 (String.toInt value)
                maxLength = if intValue > 0 then Just intValue else Nothing
            in
             ({model | maxLengthFilter = maxLength } , Cmd.none)

        Filter -> 
            let
                articles = sort (filterArticles model model.allArticles)
            in
                ({model| articles = articles}, Cmd.batch [ randomizeArticles articles, saveModel])
        SaveModel -> (model, saveData model)