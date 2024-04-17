module View.MainView exposing (..)

import Dict
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Maps.MapSizes exposing (mapCellSquareSizeInPixelString, mapHeightInPxString, mapWidthInPxString)
import Messages exposing (Msg)
import Models.MainModel exposing (MainModel, Map, MapCell)
import Svg exposing (Svg)
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
    Svg.svg
        [ style "height" mapHeightInPxString
        , style "width" mapWidthInPxString
        , style "background-color" "black"
        ]
        [ Svg.g [] (renderMapCells model.currentMap)
        ]


renderMapCells : Map -> List (Svg Msg)
renderMapCells map =
    Dict.foldl renderMapCell [] map.mapCells


renderMapCell : String -> MapCell -> List (Svg Msg) -> List (Svg Msg)
renderMapCell _ mapCell svgList =
    let
        attributes =
            baseGridCellAttributes mapCell
    in
    Svg.rect attributes [] :: svgList


baseGridCellAttributes : MapCell -> List (Attribute msg)
baseGridCellAttributes mapCell =
    [ SvgAttr.clipPath horizontalGridPolygon
    , SvgAttr.width mapCellSquareSizeInPixelString
    , SvgAttr.height mapCellSquareSizeInPixelString
    , SvgAttr.x mapCell.startWidthInPx
    , SvgAttr.y mapCell.startHeightInPx
    , SvgAttr.fill "white"
    ]


horizontalGridPolygon : String
horizontalGridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


makePxStringFromFloat : Float -> String
makePxStringFromFloat floatNumber =
    String.fromFloat floatNumber ++ "px"
