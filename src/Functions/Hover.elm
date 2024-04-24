module Functions.Hover exposing (..)

import Dict exposing (Dict)
import Functions.Coordinates exposing (getNextMapCoordinate)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe)
import Functions.Direction exposing (getNextDirectionForGoingAround)
import Functions.Movement exposing (getGoAroundStartSpot)
import Functions.ToString exposing (cellContentToString)
import Models.MainModel exposing (Error, Map, MapCell, MapCoordinate)
import Models.Types exposing (Color(..), GridDirection(..), MapCellContent(..))


trySetMovementPathToHoveredCell : MapCoordinate -> Map -> Result Error Map
trySetMovementPathToHoveredCell mapCoordinate map =
    -- hovering is only on out of combat
    -- only empty cells, which have steps, can be hovered
    -- so this means there must a possible path towards hovered cell.
    -- we set this path in our temp map, so that it can be instantly removed.
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

                    Just steps ->
                        -- everything is oke. So we can set color.
                        let
                            updatedMapCell =
                                { mapCell | cellColor = HoveredColor }

                            updatedMapCells =
                                insertMapCellInDictUnSafe updatedMapCell mapCells

                            updatedMapCellsWithPathResult =
                                trySetMovementPathRecursive steps mapCoordinate updatedMapCells
                        in
                        case updatedMapCellsWithPathResult of
                            Err err ->
                                Err err

                            Ok updatedMapCellsWithPath ->
                                Ok { map | tempMapCells = Just updatedMapCellsWithPath }


trySetMovementPathRecursive : Int -> MapCoordinate -> Dict String MapCell -> Result Error (Dict String MapCell)
trySetMovementPathRecursive stepsForThisMapCoordinate currentMapCoordinate mapCellDict =
    let
        stepsToFind =
            stepsForThisMapCoordinate - 1
    in
    if stepsToFind == 0 then
        -- We are done, and path is set in our map dict.
        Ok mapCellDict

    else
        let
            goAroundStartSpot =
                getGoAroundStartSpot currentMapCoordinate

            nextMapCellResult =
                tryFindNextMapCellForSettingMovePath stepsToFind goAroundStartSpot mapCellDict Right True
        in
        case nextMapCellResult of
            Err err ->
                Err err

            Ok nextMapCell ->
                let
                    updatedMapCell =
                        { nextMapCell | cellColor = PartOfPathColor }

                    updatedMapCells =
                        insertMapCellInDictUnSafe updatedMapCell mapCellDict
                in
                trySetMovementPathRecursive stepsToFind nextMapCell.mapCoordinate updatedMapCells


tryFindNextMapCellForSettingMovePath : Int -> MapCoordinate -> Dict String MapCell -> GridDirection -> Bool -> Result Error MapCell
tryFindNextMapCellForSettingMovePath stepsToFind goAroundMapCoordinate mapCellDict currentDirection isStart =
    if currentDirection == Right && not isStart then
        Err
            { method = "Functions.Hover.tryFindNextMapCellForSettingMovePath"
            , error = "No cell found with needed movement to make path, " ++ String.fromInt stepsToFind
            }

    else
        let
            getMapCellResult =
                tryGetMapCellFromMapCellDict goAroundMapCoordinate mapCellDict

            nextDirection =
                getNextDirectionForGoingAround currentDirection

            nextMapCoordinate =
                getNextMapCoordinate currentDirection goAroundMapCoordinate
        in
        case getMapCellResult of
            Err _ ->
                tryFindNextMapCellForSettingMovePath stepsToFind nextMapCoordinate mapCellDict nextDirection False

            Ok mapCell ->
                case mapCell.stepsToMoveTowards of
                    Nothing ->
                        tryFindNextMapCellForSettingMovePath stepsToFind nextMapCoordinate mapCellDict nextDirection False

                    Just steps ->
                        if steps == stepsToFind then
                            Ok mapCell

                        else
                            tryFindNextMapCellForSettingMovePath stepsToFind nextMapCoordinate mapCellDict nextDirection False
