module Model exposing (..)

import Date

type alias Model =
    {
        loginData : Maybe LoginData,
        url : String,
        articles : List Article,
        randomArticle : Maybe Article,
        tagsFilter : Maybe String,
        allArticles : List Article,
        maxLengthFilter : Maybe Int
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
        added: Maybe String,
        length: Maybe Int
    }

