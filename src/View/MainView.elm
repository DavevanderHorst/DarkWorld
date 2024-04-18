module View.MainView exposing (..)

import Dict
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Maps.MapSizes exposing (mapCellSquareSizeInPixelString, mapHeightInPxString, mapWidthInPxString)
import Messages exposing (Msg)
import Models.MainModel exposing (MainModel, Map, MapCell, MapCellContent(..))
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
        baseGridCellAttributes =
            makeBaseGridCellAttributes mapCell
    in
    case mapCell.content of
        Empty ->
            let
                attributes =
                    SvgAttr.fill "white" :: baseGridCellAttributes
            in
            Svg.rect attributes [] :: svgList

        Hero ->
            let
                attributes =
                    SvgAttr.xlinkHref "Images/swordsman.png" :: baseGridCellAttributes
            in
            Svg.image attributes [] :: svgList


makeBaseGridCellAttributes : MapCell -> List (Attribute msg)
makeBaseGridCellAttributes mapCell =
    [ SvgAttr.clipPath horizontalGridPolygon
    , SvgAttr.width mapCellSquareSizeInPixelString
    , SvgAttr.height mapCellSquareSizeInPixelString
    , SvgAttr.x mapCell.startWidthInPx
    , SvgAttr.y mapCell.startHeightInPx
    ]


horizontalGridPolygon : String
horizontalGridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


makePxStringFromFloat : Float -> String
makePxStringFromFloat floatNumber =
    String.fromFloat floatNumber ++ "px"
