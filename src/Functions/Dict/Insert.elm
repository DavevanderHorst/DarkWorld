module Functions.Dict.Insert exposing (..)

import Dict exposing (Dict)
import Functions.Dict.Basic exposing (createMapCellDictKey)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.ToString exposing (cellContentToString)
import Models.MainModel exposing (Error, MapCell, MapCellContent(..), MapCoordinate)


insertMapCellInDictUnSafe : MapCell -> Dict String MapCell -> Dict String MapCell
insertMapCellInDictUnSafe mapCell gridCellDict =
    -- Unsafe, only use when your 100% sure cell does not yet exists or if you want to overwrite complete cell
    Dict.insert (createMapCellDictKey mapCell.mapCoordinate) mapCell gridCellDict


trySetHeroInMapCellDict : MapCoordinate -> Dict String MapCell -> Result Error (Dict String MapCell)
trySetHeroInMapCellDict roomCoordinate gridCellDict =
    let
        getMapCellResult =
            tryGetMapCellFromMapCellDict roomCoordinate gridCellDict
    in
    case getMapCellResult of
        Err err ->
            Err err

        Ok mapCell ->
            if mapCell.content == Empty then
                Ok (updateGridCellDict roomCoordinate setCellStateToHero gridCellDict)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInMapCellDict"
                    , error = "Cant set hero, cell is not empty : " ++ cellContentToString mapCell.content
                    }


setCellStateToHero : Maybe MapCell -> Maybe MapCell
setCellStateToHero =
    Maybe.map
        (\old -> { old | content = Hero })


updateGridCellDict : MapCoordinate -> (Maybe MapCell -> Maybe MapCell) -> Dict String MapCell -> Dict String MapCell
updateGridCellDict roomCoordinate function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update (createMapCellDictKey roomCoordinate) function gridCellDict
