module Functions.ToString exposing (..)

import Models.MainModel exposing (MapCoordinate)
import Models.Types exposing (MapCellContent(..), MonsterType(..))


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

        Monster monsterType ->
            "Monster " ++ monsterTypeToString monsterType


monsterTypeToString : MonsterType -> String
monsterTypeToString monsterType =
    case monsterType of
        Dummy ->
            "Dummy"
