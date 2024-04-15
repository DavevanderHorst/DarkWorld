module View.MainView exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Messages exposing (Msg)
import Models.MainModel exposing (MainModel)


view : MainModel -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100%"
        , style "width" "100%"
        ]
        [ div
            [ style "height" "400px"
            , style "width" "800px"
            , style "background-color" "black"
            ]
            []
        ]
