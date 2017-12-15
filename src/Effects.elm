module Effects exposing (..)

import Http
import Json.Decode as Json
import Navigation
import Date
import Random
import Random.List
import Model exposing (Model, LoginData, Article)
import Messages exposing (Msg)
import Task

apiUrl : String
apiUrl = "https://2gf3hu5hsh.execute-api.us-east-1.amazonaws.com/dev"

getRequestToken : Cmd Msg
getRequestToken=
    let 
     url = apiUrl ++  "/getRequestToken"
    in
     Http.send Messages.NewRequestToken (Http.get url decodeRequestToken)

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
     Http.send Messages.LoggedIn (Http.get url decodeAccessToken)

decodeAccessToken : Json.Decoder LoginData
decodeAccessToken =
    Json.map2 LoginData (Json.field "userName" Json.string) (Json.field "accessToken" Json.string)

downloadArticles: String -> Cmd Msg
downloadArticles accessToken =
    let 
     url =  apiUrl ++ "/getArticles?accesstoken=" ++ accessToken
    in
     Http.send Messages.DownloadedArticles (Http.get url decodeArticles)


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

decodeDate : String -> Maybe String
decodeDate date = 
    let result = Date.fromString date
    in
     case result of
     Ok date -> Just (toString  date)
     Err _ -> Nothing

decodeInt : Maybe String -> Maybe Int
decodeInt value = 
    case value of
    Just v -> 
        case String.toInt v of
        Ok i -> Just i
        Err _ -> Nothing
    Nothing -> Nothing
    
randomizeArticles: List Article -> Cmd Msg
randomizeArticles articles =
    Random.generate Messages.RandomizedArticles (Random.List.shuffle articles)

filterArticles: Model -> List Article -> List Article
filterArticles model articles=
    let
        tags = 
            case model.tagsFilter of
            Just tagsValue -> List.filter isValidTag (String.split ";" tagsValue)
            Nothing -> []
        tagsPredicate = isArticleWithTags tags
        maxLengthPredicate = isArticleWithMaxLength model.maxLengthFilter
    in
      List.filter maxLengthPredicate (List.filter tagsPredicate articles)
    
isArticleWithTags: List String -> Article -> Bool
isArticleWithTags tags article=
    List.isEmpty tags || List.all (contains article.tags) tags

isArticleWithMaxLength: Maybe Int -> Article -> Bool
isArticleWithMaxLength maxLength article=
    case maxLength of
    Just max -> 
        case article.length of
        Just l -> l <= max
        Nothing ->True
    Nothing -> True

isValidTag: String -> Bool
isValidTag tag=
    String.length tag > 0

contains: List String -> String -> Bool
contains tags tag=
    if tag == "_untagged_" then List.isEmpty tags else List.member tag tags

sort: List Article -> List Article
sort articles =
    List.sortBy sortValue articles

sortValue: Article -> Float
sortValue article =
    case article.added of
    Just d -> 
        case Date.fromString d of
        Ok d -> Date.toTime d
        Err _ -> 0
    Nothing -> 0

filter: Cmd Msg
filter = Task.succeed Messages.Filter |> Task.perform identity