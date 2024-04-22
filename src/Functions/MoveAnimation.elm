module Functions.MoveAnimation exposing (..)

import Dict exposing (Dict)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Models.MainModel exposing (Error, MapCell, MapCoordinate)
import Models.Point exposing (Point)
import Models.Types exposing (AnimationType(..))


tryMakeMoveAnimation : MapCoordinate -> List MapCoordinate -> Dict String MapCell -> Result Error AnimationType
tryMakeMoveAnimation heroSpot movementPathMapCoordinates mapCellDict =
    if List.isEmpty movementPathMapCoordinates then
        Err
            { method = "Functions.MoveAnimation.makeMoveAnimation"
            , error = "The list with map coordinates for moving the hero is empty."
            }

    else
        let
            heroMapCellResult =
                tryGetMapCellFromMapCellDict heroSpot mapCellDict
        in
        case heroMapCellResult of
            Err err ->
                Err err

            Ok heroMapCell ->
                let
                    animationPointListResult =
                        makeAnimationPointList movementPathMapCoordinates mapCellDict
                in
                case animationPointListResult of
                    Err err ->
                        Err err

                    Ok animationPointList ->
                        Ok (AnimationMove (makeAnimationPointForMapCell heroMapCell) animationPointList)


getGridCellAndMakePoint : Dict String MapCell -> MapCoordinate -> Result Error (List Point) -> Result Error (List Point)
getGridCellAndMakePoint mapCellDict mapCoordinate result =
    case result of
        Err err ->
            Err err

        Ok pointList ->
            let
                getMapCellResult =
                    tryGetMapCellFromMapCellDict mapCoordinate mapCellDict
            in
            case getMapCellResult of
                Err err ->
                    Err err

                Ok gridCell ->
                    Ok (makeAnimationPointForMapCell gridCell :: pointList)


makeAnimationPointForMapCell : MapCell -> Point
makeAnimationPointForMapCell mapCell =
    { x = toFloat mapCell.startWidth, y = toFloat mapCell.startHeight }


makeAnimationPointList : List MapCoordinate -> Dict String MapCell -> Result Error (List Point)
makeAnimationPointList coordinatesList mapCellDict =
    List.foldr (getGridCellAndMakePoint mapCellDict) (Ok []) coordinatesList
