module View.MainView exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Messages exposing (Msg)
import Models.MainModel exposing (MainModel)
import Svg
import Svg.Attributes as SvgAttr


view : MainModel -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" (makePxStringFromFloat model.screenDimensions.height)
        , style "width" (makePxStringFromFloat model.screenDimensions.width)
        ]
        [ case model.error of
            Just error ->
                div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "font-size" "40px"
                    ]
                    [ div [] [ text ("what method : " ++ error.method) ]
                    , div [] [ text ("error" ++ error.error) ]
                    ]

            Nothing ->
                mapView model
        ]


mapView : MainModel -> Html Msg
mapView model =
    div
        [ style "height" "400px"
        , style "width" "800px"
        , style "background-color" "black"
        ]
        [ Svg.svg []
            [ Svg.g []
                [ Svg.rect
                    [ SvgAttr.fill "white"
                    , SvgAttr.clipPath horizontalGridPolygon
                    , SvgAttr.width "20px"
                    , SvgAttr.height "20px"
                    , SvgAttr.x "20"
                    , SvgAttr.y "50"
                    ]
                    []
                ]
            ]
        ]


horizontalGridPolygon : String
horizontalGridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


makePxStringFromFloat : Float -> String
makePxStringFromFloat floatNumber =
    String.fromFloat floatNumber ++ "px"
