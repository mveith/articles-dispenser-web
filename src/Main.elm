module Main exposing (..)

import Navigation
import Model exposing (Model)
import Update exposing (update)
import Effects
import Messages exposing (Msg)
import View exposing (view)

---- PROGRAM ----

main : Program Never Model Msg
main =
    Navigation.program Messages.UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

init :Navigation.Location -> ( Model, Cmd Msg )
init location =
    case location.pathname of
    "/authorized"-> 
        let
         authorizedRequestToken = List.head (List.reverse (String.split "=" location.search))
        in
         case authorizedRequestToken of
         Just token -> ( Model Nothing location.origin [] Nothing, Effects.getAccessToken token)
         Nothing -> ( Model Nothing location.origin [] Nothing, Cmd.none )
    _-> ( Model Nothing location.origin [] Nothing, Cmd.none )
