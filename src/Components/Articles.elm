module Components.Articles exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, class, href, target, id, attribute)
import Date
import Date.Format
import Model exposing (Model, Article)
import Messages exposing (..)

articlesView : Model -> Html Msg
articlesView model =
    Html.section [class "articles h-100"]
        [                    
            div [class "container"]
            [
                div [class "update"] [ downloadArticlesButton ],                
                div [class "dispenser-buttons" ][ randomArticleButton model.randomArticle ],
                div [class "list-group article-rows"] (List.indexedMap articleRow (List.reverse model.articles))
            ]
        ]

articleRow: Int -> Article -> Html Msg
articleRow index article=
    Html.a [class "list-group-item list-group-item-action flex-column align-items-start"] 
    [
        Html.h5 [class "mb-1 word-wrap article-title"][text article.title],
        Html.p [class "text-left mb-0"] 
        [
            Html.small [] 
            [
                Html.strong [] [text "Tags: "], 
                (text (String.join ", " article.tags))
            ]
        ],
        Html.p [class "text-left mb-1"] 
        [
            Html.small [] 
            [
                Html.strong [] [text "Length: "], 
                (text (lengthView article.length))
            ]
        ],
        Html.span [ class "float-left article-links"] 
        [
            Html.a [href article.url, target "_blank"] [Html.i [class "fa fa-link"][] ],
            text " ",
            Html.a [href (readLink article), target "_blank"] [Html.i [class "fa fa-get-pocket"][] ]
        ],
        Html.span [ class "float-right"] 
        [
            Html.small [] [(text (dateView article.added))]
        ]
    ]

downloadArticlesButton : Html Msg
downloadArticlesButton =
    Html.a 
    [ 
        onClick DownloadArticles, 
        class "btn btn-outline-primary mb-2", 
        attribute "role" "button", 
        attribute "aria-pressed" "true" 
    ]
    [ text "Download articles"]

dateView : Maybe Date.Date -> String
dateView date =
    case date of
    Just d -> Date.Format.format "%d/%m/%Y" d
    Nothing -> ""

randomArticleButton : Maybe Article -> Html Msg
randomArticleButton article=
    case article of
    Just a -> 
        div [class "mb-1 text-left"] 
            [
                Html.a 
                [ 
                    onClick GenerateRandomArticle, 
                    href (readLink a), 
                    target "_blank", 
                    class "btn btn-outline-secondary", 
                    attribute "role" "button", 
                    attribute "aria-pressed" "true" 
                ]
                [ text "Random"]
            ]
    Nothing -> div[][]


lengthView : Maybe Int -> String
lengthView length =
    case length of
    Just v -> (toString v) ++ " words"
    Nothing -> ""


readLink : Article -> String
readLink article =
    "https://getpocket.com/a/read/" ++ article.id

