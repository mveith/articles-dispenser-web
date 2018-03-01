module Messages exposing (..)

import Http
import Navigation
import Model exposing (Model, LoginData, Article)

type Msg
    = Login
    | NewRequestToken (Result Http.Error String)
    | UrlChange Navigation.Location
    | LoggedIn (Result Http.Error LoginData)
    | DownloadedArticles (Result Http.Error (List Article))
    | GenerateRandomArticle
    | RandomizedArticles (List Article)
    | TagsFilter String
    | TagsFilterAndExecute String
    | MaxLengthFilter String
    | Filter
    | SaveModel