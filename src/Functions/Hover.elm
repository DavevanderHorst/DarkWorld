module Functions.Hover exposing (..)

import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe)
import Functions.ToString exposing (cellContentToString)
import Models.MainModel exposing (Error, Map, MapCoordinate)
import Models.Types exposing (Color(..), MapCellContent(..))


trySetHoverColorInTempMapCells : MapCoordinate -> Map -> Result Error Map
trySetHoverColorInTempMapCells mapCoordinate map =
    -- only possible when no combat, if cell is empty and can be moved too
    -- TODO check for combat when that is created
    let
        mapCells =
            map.mapCells

        getMapCellResult =
            tryGetMapCellFromMapCellDict mapCoordinate mapCells
    in
    case getMapCellResult of
        Err error ->
            Err error

        Ok mapCell ->
            if mapCell.content /= Empty then
                Err
                    { method = "Functions.Hover.trySetHoverColorInTempMapCells"
                    , error = "Cell cant be hovered, its not empty : " ++ cellContentToString mapCell.content
                    }

            else
                case mapCell.stepsToMoveTowards of
                    Nothing ->
                        Err
                            { method = "Functions.Hover.trySetHoverColorInTempMapCells"
                            , error = "Cell cant be hovered, it cant be moved too"
                            }

                    Just _ ->
                        -- everything is oke. So we can set color.
                        let
                            updatedMapCell =
                                { mapCell | cellColor = HoveredColor }

                            updatedMapCells =
                                insertMapCellInDictUnSafe updatedMapCell mapCells

                            updatedMap =
                                { map | tempMapCells = Just updatedMapCells }
                        in
                        Ok updatedMap
