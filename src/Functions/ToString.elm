module Functions.ToString exposing (..)

import Models.MainModel exposing (MapCellContent(..), MapCoordinate)


mapCoordinateToString : MapCoordinate -> String
mapCoordinateToString coordinate =
    "(" ++ String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber ++ ")"


cellContentToString : MapCellContent -> String
cellContentToString content =
    case content of
        Empty ->
            "Empty"

        Hero ->
            "Hero"
