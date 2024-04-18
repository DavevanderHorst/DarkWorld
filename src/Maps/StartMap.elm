module Maps.StartMap exposing (..)

import BasicFunctions exposing (isEvenIntNumber)
import Dict exposing (Dict)
import Maps.DictFunctions exposing (createMapCellDictKey)
import Maps.MapCreationFunctions exposing (makeMapCoordinateList)
import Maps.MapSizes exposing (blackBackgroundMapMargin, evenRowHorizontalBaseShift, halfMapCellSquareSize, halfRoomBetweenHorizontalCells, mapCellSquareSize, mapCellTotalHorizontalWidth, mapCellTotalVerticalHeight, quarterMapCellSquareSize, roomBetweenHorizontalCells)
import Models.MainModel exposing (Map, MapCell, MapCoordinate)


makeStartMap : Map
makeStartMap =
    let
        mapCells =
            List.foldl generateMapCell Dict.empty makeMapCoordinateList
    in
    Map 1 mapCells


generateMapCell : MapCoordinate -> Dict String MapCell -> Dict String MapCell
generateMapCell mapCoordinate gridCellDict =
    let
        ( columnNumber, rowNumber ) =
            ( mapCoordinate.columnNumber, mapCoordinate.rowNumber )

        ( stepsX, stepsY ) =
            ( columnNumber - 1, rowNumber - 1 )

        gridX =
            let
                baseX =
                    -- total size of a cell * (column number - 1) + black margin of background
                    (mapCellTotalHorizontalWidth * stepsX) + blackBackgroundMapMargin
            in
            if isEvenIntNumber rowNumber then
                -- the even rows we need to shift right, so that they fall nice together
                baseX + evenRowHorizontalBaseShift

            else
                baseX

        gridY =
            -- start + number of rows that went before + padding from the base rect
            --              + black margin of background
            (mapCellTotalVerticalHeight * stepsY) + blackBackgroundMapMargin

        newMapCell : MapCell
        newMapCell =
            { startWidthInPx = String.fromInt gridX ++ "px"
            , startHeightInPx = String.fromInt gridY ++ "px"
            , mapCoordinate = mapCoordinate
            }
    in
    insertMapCellInDict newMapCell gridCellDict


insertMapCellInDict : MapCell -> Dict String MapCell -> Dict String MapCell
insertMapCellInDict mapCell gridCellDict =
    Dict.insert (createMapCellDictKey mapCell.mapCoordinate) mapCell gridCellDict
