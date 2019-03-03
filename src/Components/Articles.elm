module Components.Articles exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (src, class, href, target, id, attribute, value, type_, placeholder)
import Model exposing (Model, Article)
import Messages exposing (..)
import Json.Decode as Json
import Set
import DateFormat
import Time
import Iso8601

articlesView : Model -> Html Msg
articlesView model =
    Html.section [class "articles h-100"]
        [                    
            div [class "container"]
            [
                div [class "row"]
                (if (not (List.isEmpty model.allArticles)) then 
                    [                        
                        div [class "col-lg-9" ]
                        [
                            div [class "filters text-left" ]
                            [
                                div [ class "form-group"] 
                                [
                                    Html.label [Html.Attributes.for "tagsInput"] [text "Tags:"],
                                    Html.input [id "tagsInput", value (Maybe.withDefault "" model.tagsFilter), type_ "text", placeholder "", onInput Messages.TagsFilter, class "form-control", attribute "describedBy" "tagsHelp", onEnter Filter] [],
                                    Html.small [id "tagsHelp", class "form-text text-muted"]
                                    [
                                        text "Tags separated by a semicolon. For untagged only articles use ", 
                                        Html.a [onClick (TagsFilter "_untagged_"), class "clickable" ][ Html.i [] [text "_untagged_"]], 
                                        text "."
                                    ]
                                ],
                                div [ class "form-group"] 
                                [
                                    Html.label [Html.Attributes.for "maxLengthInput"] [text "Max length:"],
                                    Html.input [id "maxLengthInput", value (Maybe.withDefault "" (Maybe.map String.fromInt model.maxLengthFilter)), type_ "number", onInput Messages.MaxLengthFilter, class "form-control", onEnter Filter] []
                                ],
                                div [ class "form-group"]
                                [
                                    Html.a 
                                    [ 
                                        onClick Filter,
                                        class "btn btn-outline-secondary text-center", 
                                        attribute "role" "button", 
                                        attribute "aria-pressed" "true" 
                                    ]
                                    [ text "Filter"]
                                ]
                            ],
                            div [class "dispenser-buttons mb-2" ][ randomArticleButton model.randomArticle ],
                            div [class "ordering-radios mb-2 text-center"] [
                                Html.fieldset [][
                                    radio "Newest first" (model.ordering == Model.Newest) (Messages.ChangeOrder Model.Newest),
                                    radio "Oldest first" (model.ordering == Model.Oldest) (Messages.ChangeOrder Model.Oldest),
                                    radio "Longest first" (model.ordering == Model.Longest) (Messages.ChangeOrder Model.Longest),
                                    radio "Shortest first" (model.ordering == Model.Shortest) (Messages.ChangeOrder Model.Shortest),
                                    radio "Randomized" (model.ordering == Model.Random) (Messages.ChangeOrder Model.Random)
                                ]
                            ], 
                            div [class "list-group article-rows"] (List.indexedMap articleRow (List.reverse model.articles))
                        ],
                        div [class "statistics col-lg-3"] 
                        [ 
                            h1 [] [text  "Statistics:"],
                            Html.div [] 
                            [
                                Html.strong [] [text "Total: "],
                                text (String.fromInt (List.length model.allArticles) ++ " articles")
                            ],
                            Html.div [] 
                            (if (List.length model.allArticles) == (List.length model.articles) then [] else
                            [
                                Html.strong [] [text "Filtered: "],
                                text (String.fromInt (List.length model.articles) ++ " articles")
                            ]),
                            Html.br [][],
                            Html.div []
                            [
                                Html.strong [] [text "Tags: "],
                                Html.br [][],
                                Html.div [class "tag-stats" ] (tags model.allArticles)
                            ]
                        ]
                    ]
                else [])
            ]
        ]

articleRow: Int -> Article -> Html Msg
articleRow index article=
    Html.div [class "list-group-item list-group-item-action flex-column align-items-start"] 
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

dateView : Maybe String -> String
dateView date =    
    case date of
    Just d -> 
        case Iso8601.toTime d of 
        Ok dateTime -> formatDate dateTime
        Err _ -> ""
    Nothing -> ""

randomArticleButton : Maybe Article -> Html Msg
randomArticleButton article=
    case article of
    Just a -> 
        div [class "text-left"] 
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
                [ text "Open random"]
            ]
    Nothing -> div[][]

radio : String -> Bool -> Msg -> Html Msg
radio value isChecked msg =
  Html.label
    [ ]
    [ 
        Html.input 
        [ 
            type_ "radio",
            onInput (\_ -> msg), 
            Html.Attributes.checked isChecked 
        ] [],
        text value
    ]

lengthView : Maybe Int -> String
lengthView length =
    case length of
    Just v -> 
        case v of
        0 -> ""
        l ->
            let
                minutes =  ceiling ((toFloat l) / 225)
                units = case minutes of 
                    1 -> "minute"
                    _ -> "minutes"
            in String.concat [String.fromInt l, " words ", "(", String.fromInt minutes, " ", units, ")"]
    Nothing -> ""


readLink : Article -> String
readLink article =
    "https://getpocket.com/a/read/" ++ article.id

onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code = if code == 13 then Json.succeed msg else Json.fail "not ENTER"
    in        
        Html.Events.on "keydown" (Json.andThen isEnter Html.Events.keyCode)

tags : List Article ->  List (Html Msg)
tags articles =
    let
        tagsList = 
            ("_untagged_", articles |> List.filter(\a -> List.isEmpty a.tags)|> List.length) ::
            (articles 
            |> List.concatMap (\a -> a.tags) 
            |> Set.fromList 
            |> Set.toList 
            |> List.map (\t -> (t, articles |> List.filter(\a -> List.member t a.tags)|> List.length)))
    in
        tagsList 
        |> List.map (\(t, c) -> [Html.span [class "tag-label clickable", onClick (TagsFilterAndExecute t)][text t] ,Html.span [class "tag-items-count"] [text (String.fromInt c)]]) 
        |> List.concat

formatDate : Time.Posix -> String
formatDate dateTime=
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text "/"
        , DateFormat.monthNumber
        , DateFormat.text "/"
        , DateFormat.yearNumber
        ]
        Time.utc
        dateTime
