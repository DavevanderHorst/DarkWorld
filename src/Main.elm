module Main exposing (init, main, update)

import Browser
import Browser.Dom
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel, ScreenDimensions, startMainModel)
import Task
import View.MainView exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( MainModel, Cmd Msg )
init _ =
    ( startMainModel
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            let
                newScreenDimensions =
                    ScreenDimensions viewPort.viewport.width viewPort.viewport.height
            in
            ( { model | screenDimensions = newScreenDimensions }, Cmd.none )


subscriptions : MainModel -> Sub Msg
subscriptions model =
    Sub.none
