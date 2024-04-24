module View.MainView exposing (..)

import Dict
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Maps.MapSizes exposing (mapCellSquareSizeInPixelString, mapHeightInPxString, mapWidthInPxString)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel, Map, MapCell)
import Models.Types exposing (CellMovementState(..), MapCellContent(..), MonsterType(..))
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import View.Colors exposing (getHtmlColor, heroMapCellColor, monsterAgroColorString)


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
                    [ div [] [ text ("What method : " ++ error.method) ]
                    , div [] [ text ("Full error : " ++ error.error) ]
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
    let
        mapCellsToUse =
            case map.tempMapCells of
                Nothing ->
                    map.mapCells

                Just tempMapCells ->
                    tempMapCells
    in
    Dict.foldl (renderMapCell map.cellMovementState) [] mapCellsToUse


renderMapCell : CellMovementState -> String -> MapCell -> List (Svg Msg) -> List (Svg Msg)
renderMapCell cellMovementState _ mapCell svgList =
    let
        baseGridCellAttributes =
            makeBaseGridCellAttributes mapCell
    in
    case mapCell.content of
        Empty ->
            let
                cellColor =
                    getHtmlColor mapCell.cellColor

                baseAttributes =
                    SvgAttr.fill cellColor :: baseGridCellAttributes

                readyAttributes =
                    case cellMovementState of
                        Active ->
                            Svg.Events.onMouseOver (MapCellIsHovered mapCell.mapCoordinate)
                                :: Svg.Events.onMouseOut MapCellIsLeft
                                :: Svg.Events.onClick (MapCellIsClicked mapCell.mapCoordinate)
                                :: baseAttributes

                        Passive ->
                            baseAttributes
            in
            --let
            --    movementText =
            --        case mapCell.stepsToMoveTowards of
            --            Nothing ->
            --                "ZZ"
            --
            --            Just steps ->
            --                String.fromInt steps
            --in
            Svg.rect readyAttributes []
                --:: Svg.g
                --    [ SvgAttr.width mapCellSquareSizeInPixelString
                --    , SvgAttr.height mapCellSquareSizeInPixelString
                --    , SvgAttr.x (makePxStringFromInt mapCell.startWidth)
                --    , SvgAttr.y (makePxStringFromInt mapCell.startHeight)
                --    ]
                --    [ Svg.text_
                --        [ SvgAttr.fill "none"
                --        , SvgAttr.stroke "red"
                --        , SvgAttr.textAnchor "middle"
                --        , SvgAttr.dominantBaseline "middle"
                --        , SvgAttr.x (makePxStringFromInt (mapCell.startWidth + 26))
                --        , SvgAttr.y (makePxStringFromInt (mapCell.startHeight + 26))
                --        ]
                --        [ text movementText ]
                --    ]
                :: svgList

        Hero ->
            let
                backGroundColorRectAttributes =
                    SvgAttr.fill heroMapCellColor :: baseGridCellAttributes

                imageAttributes =
                    SvgAttr.xlinkHref "Images/swordsmanNoBg.png" :: baseGridCellAttributes
            in
            -- To set the background color we need to add 2 svg s
            Svg.rect backGroundColorRectAttributes [] :: Svg.image imageAttributes [] :: svgList

        Monster monsterType ->
            let
                backGroundColorRectAttributes =
                    SvgAttr.fill monsterAgroColorString :: baseGridCellAttributes

                imageAttributes =
                    case monsterType of
                        Dummy ->
                            SvgAttr.xlinkHref "Images/dummyNoBg.png" :: baseGridCellAttributes
            in
            -- To set the background color we need to add 2 svg s
            Svg.rect backGroundColorRectAttributes [] :: Svg.image imageAttributes [] :: svgList


makeBaseGridCellAttributes : MapCell -> List (Attribute msg)
makeBaseGridCellAttributes mapCell =
    [ SvgAttr.clipPath horizontalGridPolygon
    , SvgAttr.width mapCellSquareSizeInPixelString
    , SvgAttr.height mapCellSquareSizeInPixelString
    , SvgAttr.x (makePxStringFromInt mapCell.startWidth)
    , SvgAttr.y (makePxStringFromInt mapCell.startHeight)
    ]


horizontalGridPolygon : String
horizontalGridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


makePxStringFromFloat : Float -> String
makePxStringFromFloat floatNumber =
    String.fromFloat floatNumber ++ "px"


makePxStringFromInt : Int -> String
makePxStringFromInt intNumber =
    String.fromInt intNumber ++ "px"
