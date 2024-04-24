module Functions.Coordinates exposing (..)

import Functions.Basic exposing (differenceBetweenIntNumbers, isEvenIntNumber)
import Models.MainModel exposing (MapCoordinate)
import Models.Types exposing (GridDirection(..))



-- for fast changing room coordinates


goUp : MapCoordinate -> MapCoordinate
goUp coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1 }


goUpRight : MapCoordinate -> MapCoordinate
goUpRight coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber + 1 }


goRight : MapCoordinate -> MapCoordinate
goRight coordinate =
    { coordinate | columnNumber = coordinate.columnNumber + 1 }


goDownRight : MapCoordinate -> MapCoordinate
goDownRight coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber + 1 }


goDown : MapCoordinate -> MapCoordinate
goDown coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1 }


goDownLeft : MapCoordinate -> MapCoordinate
goDownLeft coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber - 1 }


goLeft : MapCoordinate -> MapCoordinate
goLeft coordinate =
    { coordinate | columnNumber = coordinate.columnNumber - 1 }


goUpLeft : MapCoordinate -> MapCoordinate
goUpLeft coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber - 1 }


getNextMapCoordinate : GridDirection -> MapCoordinate -> MapCoordinate
getNextMapCoordinate direction current =
    let
        isEvenRow =
            isEvenIntNumber current.rowNumber
    in
    case direction of
        Left ->
            goLeft current

        UpLeft ->
            if isEvenRow then
                goUp current

            else
                goUpLeft current

        UpRight ->
            if isEvenRow then
                goUpRight current

            else
                goUp current

        Right ->
            goRight current

        DownRight ->
            if isEvenRow then
                goDownRight current

            else
                goDown current

        DownLeft ->
            if isEvenRow then
                goDown current

            else
                goDownLeft current


areMapCoordinatesNextToEachOther : MapCoordinate -> MapCoordinate -> Bool
areMapCoordinatesNextToEachOther first second =
    if first.rowNumber == second.rowNumber then
        -- if row number is the same, then they are next to each other fi difference in column number is one
        if differenceBetweenIntNumbers first.columnNumber second.columnNumber == 1 then
            True

        else
            False

    else if differenceBetweenIntNumbers first.rowNumber second.rowNumber == 1 then
        -- if difference between row numbers is only one, then its possible that
        if isEvenIntNumber first.rowNumber then
            if first.columnNumber == second.columnNumber || first.columnNumber + 1 == second.columnNumber then
                True

            else
                False

        else if second.columnNumber == first.columnNumber || second.columnNumber + 1 == first.columnNumber then
            True

        else
            False

    else
        False
