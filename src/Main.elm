module Main exposing (..)

import Navigation
import Model exposing (Model, LoginData)
import Update exposing (update)
import Effects
import Messages exposing (Msg)
import View exposing (view)

---- PROGRAM ----

main : Program (Maybe LoginData) Model Msg
main =
    Navigation.programWithFlags Messages.UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

init : Maybe LoginData -> Navigation.Location -> ( Model, Cmd Msg )
init loginData location =
    case location.pathname of
    "/authorized"-> 
        let
         authorizedRequestToken = List.head (List.reverse (String.split "=" location.search))
        in
         case authorizedRequestToken of
         Just token -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Effects.getAccessToken token)
         Nothing -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Cmd.none )
    _-> 
        case loginData of
        Just v ->( Model (Just v) location.origin [] Nothing Nothing [] Nothing, Effects.downloadArticles v.accessToken )
        Nothing -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Cmd.none )
