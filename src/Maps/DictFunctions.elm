module Maps.DictFunctions exposing (..)

import Models.MainModel exposing (MapCoordinate)


createMapCellDictKey : MapCoordinate -> String
createMapCellDictKey mapCoordinate =
    String.fromInt mapCoordinate.columnNumber ++ "," ++ String.fromInt mapCoordinate.rowNumber
