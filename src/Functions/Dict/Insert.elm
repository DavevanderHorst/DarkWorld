module Functions.Dict.Insert exposing (..)

import Dict exposing (Dict)
import Functions.Dict.Basic exposing (createMapCellDictKey)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.ToString exposing (cellContentToString)
import Models.MainModel exposing (Error, MapCell, MapCoordinate)
import Models.Types exposing (MapCellContent(..), MonsterType)



-- HERO


trySetHeroInMapCellDict : MapCoordinate -> Dict String MapCell -> Result Error (Dict String MapCell)
trySetHeroInMapCellDict mapCoordinate mapCellDict =
    let
        getMapCellResult =
            tryGetMapCellFromMapCellDict mapCoordinate mapCellDict
    in
    case getMapCellResult of
        Err err ->
            Err err

        Ok mapCell ->
            if mapCell.content == Empty then
                Ok (updateGridCellDict mapCoordinate setCellStateToHero mapCellDict)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInMapCellDict"
                    , error = "Cant set hero, cell is not empty : " ++ cellContentToString mapCell.content
                    }


setCellStateToHero : Maybe MapCell -> Maybe MapCell
setCellStateToHero =
    Maybe.map
        (\old -> { old | content = Hero })



-- MONSTERS


trySetMonstersInMapCellDict : List ( MonsterType, MapCoordinate ) -> Dict String MapCell -> Result Error (Dict String MapCell)
trySetMonstersInMapCellDict monsters mapCellDict =
    let
        setMonstersInMapCellDictResult =
            List.foldl trySetMonsterInMapCellDict (Ok mapCellDict) monsters
    in
    case setMonstersInMapCellDictResult of
        Ok monstersInMapCellDict ->
            Ok monstersInMapCellDict

        Err error ->
            Err error


trySetMonsterInMapCellDict : ( MonsterType, MapCoordinate ) -> Result Error (Dict String MapCell) -> Result Error (Dict String MapCell)
trySetMonsterInMapCellDict ( monsterType, mapCoordinate ) mapCellDictResult =
    case mapCellDictResult of
        Err err ->
            Err err

        Ok mapCellDict ->
            let
                getMapCellResult =
                    tryGetMapCellFromMapCellDict mapCoordinate mapCellDict
            in
            case getMapCellResult of
                Err err ->
                    Err err

                Ok mapCell ->
                    if mapCell.content == Empty then
                        Ok (updateGridCellDict mapCoordinate (setCellStateToMonster monsterType) mapCellDict)

                    else
                        Err
                            { method = "Functions.Dict.Insert.trySetMonsterInMapCellDict"
                            , error = "Cant set monster, cell is not empty : " ++ cellContentToString mapCell.content
                            }


setCellStateToMonster : MonsterType -> Maybe MapCell -> Maybe MapCell
setCellStateToMonster monsterType =
    Maybe.map
        (\old -> { old | content = Monster monsterType })



-- MOVEMENT


setMapCellStepsInEmptyMapCell : Int -> MapCoordinate -> Dict String MapCell -> Dict String MapCell
setMapCellStepsInEmptyMapCell steps mapCoordinate mapCellDict =
    updateGridCellDict mapCoordinate (setEmptyToCanBeMovedTo steps) mapCellDict


setEmptyToCanBeMovedTo : Int -> Maybe MapCell -> Maybe MapCell
setEmptyToCanBeMovedTo steps =
    Maybe.map
        (\old ->
            case old.content of
                Empty ->
                    { old | stepsToMoveTowards = Just steps }

                Hero ->
                    old

                Monster _ ->
                    old
        )



-- COMMON


insertMapCellInDictUnSafe : MapCell -> Dict String MapCell -> Dict String MapCell
insertMapCellInDictUnSafe mapCell gridCellDict =
    -- Unsafe, only use when your 100% sure cell does not yet exists or if you want to overwrite complete cell
    Dict.insert (createMapCellDictKey mapCell.mapCoordinate) mapCell gridCellDict


updateGridCellDict : MapCoordinate -> (Maybe MapCell -> Maybe MapCell) -> Dict String MapCell -> Dict String MapCell
updateGridCellDict roomCoordinate function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update (createMapCellDictKey roomCoordinate) function gridCellDict
