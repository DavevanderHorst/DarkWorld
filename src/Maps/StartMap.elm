module Maps.StartMap exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (isEvenIntNumber)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe, trySetHeroInMapCellDict, trySetMonstersInMapCellDict)
import Functions.Movement exposing (trySetMovementAroundHeroInMapCells)
import Maps.MapCreationFunctions exposing (makeMapCoordinateList)
import Maps.MapSizes exposing (blackBackgroundMapMargin, evenRowHorizontalBaseShift, mapCellTotalHorizontalWidth, mapCellTotalVerticalHeight)
import Models.MainModel exposing (Error, Map, MapCell, MapCoordinate)
import Models.Types exposing (CellMovementState(..), Color(..), MapCellContent(..), MonsterType(..))


startHeroSpot : MapCoordinate
startHeroSpot =
    { columnNumber = 2, rowNumber = 5 }


startMonsterSpots : List ( MonsterType, MapCoordinate )
startMonsterSpots =
    [--( Dummy, { columnNumber = 3, rowNumber = 2 } )
     --, ( Dummy, { columnNumber = 4, rowNumber = 2 } )
     --, ( Dummy, { columnNumber = 3, rowNumber = 4 } )
     --, ( Dummy, { columnNumber = 4, rowNumber = 4 } )
     --, ( Dummy, { columnNumber = 5, rowNumber = 1 } )
     --, ( Dummy, { columnNumber = 1, rowNumber = 4 } )
     --, ( Dummy, { columnNumber = 2, rowNumber = 4 } )
    ]


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
            let
                mapCellDictWithHeroAndMonstersResult =
                    trySetMonstersInMapCellDict startMonsterSpots mapCellDictWithHero
            in
            case mapCellDictWithHeroAndMonstersResult of
                Err error ->
                    Err error

                Ok mapCellDictWithHeroAndMonsters ->
                    -- Map is ready, now we set movement steps in every spot.
                    let
                        setMovementStepsResult =
                            trySetMovementAroundHeroInMapCells startHeroSpot mapCellDictWithHeroAndMonsters
                    in
                    case setMovementStepsResult of
                        Err err ->
                            Err err

                        Ok finishedMap ->
                            Ok
                                { mapNumber = 1
                                , mapCells = finishedMap
                                , tempMapCells = Nothing
                                , cellMovementState = Active
                                }


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
            { startWidth = gridX
            , startHeight = gridY
            , mapCoordinate = mapCoordinate
            , content = Empty
            , stepsToMoveTowards = Nothing
            , cellColor = EmptyColor
            }
    in
    insertMapCellInDictUnSafe newMapCell gridCellDict
