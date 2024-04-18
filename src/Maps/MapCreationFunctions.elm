module Maps.MapCreationFunctions exposing (..)

import Functions.Basic exposing (isEvenIntNumber)
import Maps.MapSizes exposing (mapCellEvenRowTotalColumns, mapCellTotalRows, mapCellUnevenRowTotalColumns)
import Models.MainModel exposing (Map, MapCoordinate)


makeMapCoordinateList : List MapCoordinate
makeMapCoordinateList =
    List.foldl addCoordsToListForRow [] (List.range 1 mapCellTotalRows)


addCoordsToListForRow : Int -> List MapCoordinate -> List MapCoordinate
addCoordsToListForRow rowNumber gridList =
    let
        columnNumberList =
            if isEvenIntNumber rowNumber then
                List.range 1 mapCellEvenRowTotalColumns

            else
                List.range 1 mapCellUnevenRowTotalColumns
    in
    List.foldl (addCoordinateToList rowNumber) gridList columnNumberList


addCoordinateToList : Int -> Int -> List MapCoordinate -> List MapCoordinate
addCoordinateToList rowNumber columnNumber list =
    { columnNumber = columnNumber, rowNumber = rowNumber } :: list
