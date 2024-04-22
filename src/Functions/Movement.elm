module Functions.Movement exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (addToTheBackOfTheList, isEvenIntNumber)
import Functions.Coordinates exposing (getNextCoordinate, goUp, goUpLeft)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (setMapCellStepsInEmptyMapCell)
import Functions.Direction exposing (getNextDirectionForGoingAround)
import Models.MainModel exposing (Error, Map, MapCell, MapCoordinate)
import Models.Types exposing (GridDirection(..), MapCellContent(..))


trySetMovementAroundHeroInMapCells : MapCoordinate -> Dict String MapCell -> Result Error (Dict String MapCell)
trySetMovementAroundHeroInMapCells heroSpot mapCellDict =
    -- first check or if given hero spot contains hero
    let
        getHeroMapCellResult =
            tryGetMapCellFromMapCellDict heroSpot mapCellDict
    in
    case getHeroMapCellResult of
        Err err ->
            Err err

        Ok heroMapCell ->
            if heroMapCell.content /= Hero then
                Err
                    { method = "Functions.Movement.trySetMovementInMapCells"
                    , error = "The map does not contain a hero on the given hero spot coordinate"
                    }

            else
                Ok (setMovementAroundHeroSpot heroSpot mapCellDict)


setMovementAroundHeroSpot : MapCoordinate -> Dict String MapCell -> Dict String MapCell
setMovementAroundHeroSpot heroSpot mapCells =
    let
        startSpot =
            getGoAroundStartSpot heroSpot
    in
    goAroundAndSetStepsToMove [] 0 1 startSpot Right mapCells []


goAroundAndSetStepsToMove : List (Maybe ( Maybe Int, MapCell )) -> Int -> Int -> MapCoordinate -> GridDirection -> Dict String MapCell -> List MapCoordinate -> Dict String MapCell
goAroundAndSetStepsToMove currentRoundCells currentStraightSteps roundNumber currentSpot currentDirection mapCellDict notYetSetCoordinates =
    -- roundNumber is amount of times we continue into the same direction
    if currentDirection == UpRight && currentStraightSteps == roundNumber then
        let
            goAroundStartSpot =
                getGoAroundStartSpot currentSpot
        in
        if roundNumber == 1 then
            -- Round 1 we set movement automatically, so we can go to round 2
            goAroundAndSetStepsToMove currentRoundCells 0 2 goAroundStartSpot Right mapCellDict notYetSetCoordinates

        else if List.isEmpty currentRoundCells then
            -- we are done
            if List.isEmpty notYetSetCoordinates then
                mapCellDict

            else
                -- there is a list of existing coordinates that didnt have a cell with a movement value around.
                -- we go over these coordinates again, to see if anything has changed.
                handleNotYetSetCoordinates notYetSetCoordinates mapCellDict

        else
            let
                -- we made a round, so now we can try adding all coordinates for this round
                ( dictWithRoundUpdate, newNotYetSetCoordinates ) =
                    addCurrentRoundCellsToMapCellDict currentRoundCells mapCellDict notYetSetCoordinates
            in
            -- time to set our grid cells, with looking in opposite direction.
            -- we need to start next round
            goAroundAndSetStepsToMove [] 0 (roundNumber + 1) goAroundStartSpot Right dictWithRoundUpdate newNotYetSetCoordinates

    else
        let
            getMapCellResult =
                tryGetMapCellFromMapCellDict currentSpot mapCellDict

            ( nextMapCell, nextSteps, nextDirection ) =
                makeMapCellStatsForNextRound currentDirection currentStraightSteps roundNumber currentSpot
        in
        case getMapCellResult of
            Err _ ->
                -- not existing cell number, so we continue
                goAroundAndSetStepsToMove currentRoundCells nextSteps roundNumber nextMapCell nextDirection mapCellDict notYetSetCoordinates

            Ok currentGridCell ->
                -- cell exists
                -- if round 1 , then we automatically set 1 movement if its empty
                -- after round 1, we check cells around to find lowest number.
                -- We save all cells found this round, and start adding them when we made a full circle
                if roundNumber == 1 then
                    let
                        updatedDict =
                            setMapCellStepsInEmptyMapCell 1 currentGridCell.mapCoordinate mapCellDict
                    in
                    goAroundAndSetStepsToMove currentRoundCells nextSteps roundNumber nextMapCell nextDirection updatedDict notYetSetCoordinates

                else
                    let
                        maybeStepsToMove =
                            checkLowestMovementCellsAround currentSpot mapCellDict

                        updatedCurrentRoundCells =
                            Just ( maybeStepsToMove, currentGridCell ) :: currentRoundCells
                    in
                    goAroundAndSetStepsToMove updatedCurrentRoundCells nextSteps roundNumber nextMapCell nextDirection mapCellDict notYetSetCoordinates


makeMapCellStatsForNextRound : GridDirection -> Int -> Int -> MapCoordinate -> ( MapCoordinate, Int, GridDirection )
makeMapCellStatsForNextRound currentDirection currentSteps roundNumber currentSpot =
    if currentSteps == roundNumber then
        let
            nextDirection =
                getNextDirectionForGoingAround currentDirection
        in
        ( getNextCoordinate nextDirection currentSpot, 1, nextDirection )

    else
        ( getNextCoordinate currentDirection currentSpot, currentSteps + 1, currentDirection )


addCurrentRoundCellsToMapCellDict : List (Maybe ( Maybe Int, MapCell )) -> Dict String MapCell -> List MapCoordinate -> ( Dict String MapCell, List MapCoordinate )
addCurrentRoundCellsToMapCellDict currentRoundGridCells gridCellDict notYetSetCoordinates =
    let
        maybeLastSetCell =
            List.head currentRoundGridCells
    in
    case maybeLastSetCell of
        Nothing ->
            -- TODO
            ( gridCellDict, notYetSetCoordinates )

        Just lastSetCell ->
            let
                maybeTail =
                    List.tail currentRoundGridCells
            in
            case maybeTail of
                Nothing ->
                    -- TODO
                    ( gridCellDict, notYetSetCoordinates )

                Just tail ->
                    let
                        adjustedList =
                            addToTheBackOfTheList lastSetCell tail

                        tupleList =
                            List.map2 makeGridCellTupleForAdding adjustedList currentRoundGridCells
                    in
                    List.foldl addTupleMapCellsToDict ( gridCellDict, notYetSetCoordinates ) tupleList


addTupleMapCellsToDict : ( Maybe ( Maybe Int, MapCell ), Maybe Int ) -> ( Dict String MapCell, List MapCoordinate ) -> ( Dict String MapCell, List MapCoordinate )
addTupleMapCellsToDict ( toBeSet, forComparing ) ( mapCellDict, notYetSetCoordinates ) =
    case toBeSet of
        Nothing ->
            ( mapCellDict, notYetSetCoordinates )

        Just ( maybeSteps, gridCellToBeSet ) ->
            let
                maybeStepsTodo =
                    case forComparing of
                        Nothing ->
                            maybeSteps

                        Just stepsToCompare ->
                            let
                                otherCellSteps =
                                    -- stepsToCompare is lowest number around former cell, so needs to be upped.
                                    stepsToCompare + 1
                            in
                            case maybeSteps of
                                Nothing ->
                                    Just otherCellSteps

                                Just steps ->
                                    Just (min steps otherCellSteps)
            in
            case maybeStepsTodo of
                Nothing ->
                    ( mapCellDict, gridCellToBeSet.mapCoordinate :: notYetSetCoordinates )

                Just stepsToDo ->
                    let
                        updatedDict =
                            setMapCellStepsInEmptyMapCell (stepsToDo + 1) gridCellToBeSet.mapCoordinate mapCellDict
                    in
                    ( updatedDict, notYetSetCoordinates )


makeGridCellTupleForAdding : Maybe ( Maybe Int, MapCell ) -> Maybe ( Maybe Int, MapCell ) -> ( Maybe ( Maybe Int, MapCell ), Maybe Int )
makeGridCellTupleForAdding maybeToBeSet forComparing =
    case forComparing of
        Nothing ->
            ( maybeToBeSet, Nothing )

        Just ( maybeSteps, _ ) ->
            ( maybeToBeSet, maybeSteps )


handleNotYetSetCoordinates : List MapCoordinate -> Dict String MapCell -> Dict String MapCell
handleNotYetSetCoordinates notYetSetCoordinates mapCellDict =
    let
        totalCoordinates =
            List.length notYetSetCoordinates

        ( newNotYetSetCoordinates, newGridCellDict ) =
            List.foldr trySetCoordinateToCanBeMoved ( [], mapCellDict ) notYetSetCoordinates
    in
    if List.length newNotYetSetCoordinates == totalCoordinates then
        newGridCellDict

    else
        handleNotYetSetCoordinates newNotYetSetCoordinates newGridCellDict


trySetCoordinateToCanBeMoved : MapCoordinate -> ( List MapCoordinate, Dict String MapCell ) -> ( List MapCoordinate, Dict String MapCell )
trySetCoordinateToCanBeMoved mapCoordinate ( notYetSetCoordinates, mapCellDict ) =
    let
        maybeStepsToMove =
            checkLowestMovementCellsAround mapCoordinate mapCellDict
    in
    case maybeStepsToMove of
        Nothing ->
            ( mapCoordinate :: notYetSetCoordinates, mapCellDict )

        Just stepsToMove ->
            let
                updatedDict =
                    setMapCellStepsInEmptyMapCell (stepsToMove + 1) mapCoordinate mapCellDict
            in
            ( notYetSetCoordinates, updatedDict )


checkLowestMovementCellsAround : MapCoordinate -> Dict String MapCell -> Maybe Int
checkLowestMovementCellsAround currentSpot gridCellDict =
    let
        goAroundStartSpot =
            getGoAroundStartSpot currentSpot
    in
    getLowestMovement goAroundStartSpot Nothing gridCellDict Right


getLowestMovement : MapCoordinate -> Maybe Int -> Dict String MapCell -> GridDirection -> Maybe Int
getLowestMovement currentSpot maybeLowest gridCellDict direction =
    let
        currentMovement =
            getMovementValue currentSpot gridCellDict

        newLowest =
            case currentMovement of
                Nothing ->
                    maybeLowest

                Just movement ->
                    case maybeLowest of
                        Nothing ->
                            Just movement

                        Just lowest ->
                            if movement < lowest then
                                Just movement

                            else
                                maybeLowest
    in
    if direction == UpRight then
        newLowest

    else
        let
            nextDirection =
                getNextDirectionForGoingAround direction
        in
        getLowestMovement (getNextCoordinate direction currentSpot) newLowest gridCellDict nextDirection


getMovementValue : MapCoordinate -> Dict String MapCell -> Maybe Int
getMovementValue mapCoordinate gridCellDict =
    let
        currentMapCellResult =
            tryGetMapCellFromMapCellDict mapCoordinate gridCellDict
    in
    case currentMapCellResult of
        Err _ ->
            Nothing

        Ok mapCell ->
            mapCell.stepsToMoveTowards


getGoAroundStartSpot : MapCoordinate -> MapCoordinate
getGoAroundStartSpot spot =
    -- Start spot is always up left cell
    if isEvenIntNumber spot.rowNumber then
        goUp spot

    else
        goUpLeft spot
