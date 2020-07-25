module Components.Navigation exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Model exposing (LoginData)
import Messages exposing (Msg)

navigationView : LoginData -> Html Msg
navigationView loginData =
    Html.nav [class "navbar navbar-expand-lg navbar-light fixed-top navbar-shrink", id "mainNav"]
    [
        div [class "container"]
        [
            Html.a [class "navbar-brand js-scroll-trigger", href "#page-top"] [text "ARTICLES DISPENSER"],
            Html.ul [class "navbar-nav ml-auto"]
            [
                Html.li [class "nav-item"][ Html.a [class "nav-link js-scroll-trigger"][ text loginData.userName]],
                Html.li [class "nav-item logout"][ Html.a [onClick Messages.Logout, class "nav-link js-scroll-trigger"][ text "Log out"]]
            ]
        ]
    ]