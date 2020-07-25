module Components.LandingPage exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Messages exposing (Msg)


landingPageView : Model -> Html Msg
landingPageView _ =
    Html.header [class "masthead h-100"] 
    [
        div [class "container h-100"]
        [
            div [class "row h-100"] 
            [
                div [class "col-lg-12 my-auto"] 
                [
                    div [class "header-content"]
                    [
                        Html.h1 [] 
                        [
                            text "ARTICLES DISPENSER"
                        ],
                        Html.h4 [class "mb-5"] 
                        [ 
                            text "Pocket client for those who want to efficiently handle a large number of articles."
                        ],
                        Html.a [onClick Messages.Login, class "btn-rounded btn-outline btn-xl"] 
                        [
                            text "Login and start"
                        ]
                    ]
                ]
            ]
        ]
    ]