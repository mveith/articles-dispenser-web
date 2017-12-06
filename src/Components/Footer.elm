module Components.Footer exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (src, class, href, target, id)
import Model exposing (Model)
import Messages exposing (Msg)

footerView : Model -> Html Msg
footerView model =
    Html.footer []
    [
        div [ class "container"] 
        [
            Html.p [] 
            [
                text "©2017 Articles Dispenser. All Rights Reserved.",
                Html.ul [ class "list-inline"] 
                [
                    footerLink "https://twitter.com/miroveith" "fa-twitter",
                    footerLink "https://github.com/mveith/articles-dispenser-web" "fa-github"
                ]
            ]
        ]
    ]

footerLink : String -> String -> Html Msg
footerLink link ico =
    Html.li [class "list-inline-item"]
    [
        Html.a [ href link, target "_blank" ]
        [
            Html.i [class ("fa " ++ ico)][]
        ]
    ]