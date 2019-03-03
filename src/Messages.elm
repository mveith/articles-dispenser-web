module Messages exposing (..)

import Http
import Browser
import Url
import Model exposing (Model, LoginData, Article)

type Msg
    = Login
    | NewRequestToken (Result Http.Error String)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoggedIn (Result Http.Error LoginData)
    | DownloadedArticles (Result Http.Error (List Article))
    | GenerateRandomArticle
    | RandomizedArticles (List Article)
    | TagsFilter String
    | TagsFilterAndExecute String
    | MaxLengthFilter String
    | Filter
    | SaveModel
    | ChangeOrder Model.Ordering
    | UpdateArticles (List Article)