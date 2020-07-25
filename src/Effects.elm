module Effects exposing (..)

import Http
import Json.Decode as Json
import Browser.Navigation as Nav
import Random
import Random.List
import Model exposing (Model, LoginData, Article)
import Messages exposing (Msg)
import Task
import Iso8601
import Time

apiUrl : String
apiUrl = "https://2gf3hu5hsh.execute-api.us-east-1.amazonaws.com/dev"

getRequestToken : Cmd Msg
getRequestToken=
    let 
     url = apiUrl ++  "/getRequestToken"
    in
     Http.get 
        { url = url
        ,expect =  Http.expectJson Messages.NewRequestToken decodeRequestToken
        }

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
        
    Nav.load url

getAccessToken: String -> Cmd Msg
getAccessToken requestToken =
    let 
     url =  apiUrl ++ "/getAccessToken?key=" ++ requestToken
    in
     Http.get 
        { url = url
        ,expect = Http.expectJson Messages.LoggedIn decodeAccessToken
        }

decodeAccessToken : Json.Decoder LoginData
decodeAccessToken =
    Json.map2 LoginData (Json.field "userName" Json.string) (Json.field "accessToken" Json.string)

downloadArticles: String -> Cmd Msg
downloadArticles accessToken =
    let 
     url =  apiUrl ++ "/getArticles?accesstoken=" ++ accessToken
    in
     Http.get 
        { url = url
        ,expect = Http.expectJson Messages.DownloadedArticles decodeArticles
        }


decodeArticles : Json.Decoder (List Article)
decodeArticles = 
    Json.list 
        (Json.map8 Article 
            (Json.field "Url" Json.string)
            (Json.field "Id" Json.string)
            (Json.field "Title" Json.string)
            (Json.field "Excerpt" Json.string)
            (Json.field "Tags" (Json.list Json.string))
            (Json.field "Added" (Json.maybe  Json.string))
            (Json.field "IsArticle" Json.bool)
            (Json.map decodeInt (Json.maybe (Json.field "WordCount" Json.string))))

decodeInt : Maybe String -> Maybe Int
decodeInt value = 
    case value of
    Just v -> String.toInt v
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
isValidTag tag =
    String.length tag > 0

contains: List String -> String -> Bool
contains tags tag =
    if tag == "_untagged_" then List.isEmpty tags else List.member tag tags

sort: List Article -> Model.Ordering -> Cmd Msg
sort articles ordering =
    case ordering of
    Model.Newest ->  sortBy articles articleAddedMiliseconds
    Model.Longest -> sortBy articles (\a -> Maybe.withDefault 0 a.length)
    Model.Shortest -> sortBy articles (\a -> if articleLength a == 0 then -1000000 else -(articleLength a))
    Model.Oldest -> sortBy articles (\a -> -(articleAddedMiliseconds a))
    Model.Random -> Random.generate Messages.UpdateArticles (Random.List.shuffle articles)
    
sortBy: List Article -> (Article -> Int) -> Cmd Msg
sortBy articles func =
    let 
        sortedArticles = List.sortBy func articles
    in 
        Task.succeed (Messages.UpdateArticles sortedArticles) |> Task.perform identity

articleAddedMiliseconds: Article -> Int
articleAddedMiliseconds article = 
    case article.added of
    Just d ->
        case Iso8601.toTime d of
        Ok t -> Time.posixToMillis t
        Err _ -> 0
    Nothing -> 0

articleLength : Article -> Int
articleLength article =
    Maybe.withDefault 0 article.length

filter: Cmd Msg
filter = Task.succeed Messages.Filter |> Task.perform identity