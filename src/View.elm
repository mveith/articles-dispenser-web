module View exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class, href, target, id)
import Model exposing (Model)
import Messages exposing (Msg)
import Components.Footer exposing (footerView)
import Components.LandingPage exposing (landingPageView) 
import Components.Navigation exposing (navigationView)
import Components.Articles exposing (articlesView)

view : Model -> Html Msg
view model =
    div [class "h-100"] 
    [
        (case model.loginData of
            Just loginData -> 
                div [] 
                [
                    navigationView loginData,
                    articlesView model
                ]
            Nothing -> 
                landingPageView model),
        footerView model
    ]