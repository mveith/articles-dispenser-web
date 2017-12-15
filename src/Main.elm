module Main exposing (..)

import Navigation
import Model exposing (Model, LoginData)
import Update exposing (update)
import Effects
import Messages exposing (Msg)
import View exposing (view)

---- PROGRAM ----

main : Program (Maybe Model) Model Msg
main =
    Navigation.programWithFlags Messages.UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

init : Maybe Model -> Navigation.Location -> ( Model, Cmd Msg )
init model location =
    case location.pathname of
    "/authorized"-> 
        let
         authorizedRequestToken = List.head (List.reverse (String.split "=" location.search))
        in
         case authorizedRequestToken of
         Just token -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Effects.getAccessToken token)
         Nothing -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Cmd.none )
    _-> 
        case model of
        Just model ->
            case model.loginData of
            Just d -> (model, Cmd.batch [Effects.filter, Effects.downloadArticles d.accessToken])
            Nothing -> (model, Cmd.none)
        Nothing -> ( Model Nothing location.origin [] Nothing Nothing [] Nothing, Cmd.none )
