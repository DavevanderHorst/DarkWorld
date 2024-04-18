module Functions.Dict.Get exposing (..)

import Dict exposing (Dict)
import Functions.Dict.Basic exposing (createMapCellDictKey)
import Functions.ToString exposing (mapCoordinateToString)
import Models.MainModel exposing (Error, MapCell, MapCoordinate)


getMapCellFromMapCellDict : MapCoordinate -> Dict String MapCell -> Result Error MapCell
getMapCellFromMapCellDict roomCoordinate gridCellDict =
    let
        maybeGridCell =
            Dict.get (createMapCellDictKey roomCoordinate) gridCellDict
    in
    case maybeGridCell of
        Nothing ->
            Err
                { method = "Functions.Dict.Get.getMapCellFromMapCellDict"
                , error = "Room coordinate is not in our grid cell dict : " ++ mapCoordinateToString roomCoordinate
                }

        Just gridCell ->
            Ok gridCell
