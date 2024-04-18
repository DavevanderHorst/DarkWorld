module Maps.StartMap exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (isEvenIntNumber)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe, trySetHeroInMapCellDict)
import Maps.MapCreationFunctions exposing (makeMapCoordinateList)
import Maps.MapSizes exposing (blackBackgroundMapMargin, evenRowHorizontalBaseShift, mapCellTotalHorizontalWidth, mapCellTotalVerticalHeight, quarterMapCellSquareSize, roomBetweenHorizontalCells)
import Models.MainModel exposing (Error, Map, MapCell, MapCellContent(..), MapCoordinate)


startHeroSpot : MapCoordinate
startHeroSpot =
    { columnNumber = 3, rowNumber = 4 }


makeStartMap : Result Error Map
makeStartMap =
    let
        mapCellDict =
            List.foldl generateMapCell Dict.empty makeMapCoordinateList

        mapCellDictWithHeroResult =
            trySetHeroInMapCellDict startHeroSpot mapCellDict
    in
    case mapCellDictWithHeroResult of
        Err error ->
            Err error

        Ok mapCellDictWithHero ->
            Ok (Map 1 mapCellDictWithHero)


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
            , content = Empty
            }
    in
    insertMapCellInDictUnSafe newMapCell gridCellDict
