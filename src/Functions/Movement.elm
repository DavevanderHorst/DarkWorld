module Functions.Movement exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (addToTheBackOfTheList, isEvenIntNumber, returnTupleOfFirstThreeElementsOfList)
import Functions.Coordinates exposing (areMapCoordinatesNextToEachOther, getNextMapCoordinate, goUp, goUpLeft)
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (setMapCellStepsInEmptyMapCell)
import Functions.Direction exposing (getNextDirectionForGoingAround)
import Models.MainModel exposing (Error, Map, MapCell, MapCoordinate)
import Models.Types exposing (GridDirection(..), MapCellContent(..))
import Tuple exposing (first, second)


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


goAroundAndSetStepsToMove : List ( Maybe Int, MapCell ) -> Int -> Int -> MapCoordinate -> GridDirection -> Dict String MapCell -> List MapCoordinate -> Dict String MapCell
goAroundAndSetStepsToMove currentRoundCells currentStraightSteps roundNumber currentSpot currentDirection mapCellDict notYetSetCoordinates =
    -- roundNumber is amount of times we continue into the same direction
    if currentDirection == UpRight && currentStraightSteps == roundNumber then
        let
            goAroundStartSpot =
                getGoAroundStartSpot currentSpot
        in
        if roundNumber == 1 then
            -- Round 1 we set movement automatically, so currentRoundCells will be empty
            goAroundAndSetStepsToMove currentRoundCells 0 2 goAroundStartSpot Right mapCellDict notYetSetCoordinates

        else if List.isEmpty currentRoundCells then
            -- we have hit every map cell
            --  check if we still have coordinates which are not set yet.
            if List.isEmpty notYetSetCoordinates then
                -- all cells done and no more coordinates that are not set.
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
                if currentGridCell.content == Empty then
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
                                ( maybeStepsToMove, currentGridCell ) :: currentRoundCells
                        in
                        goAroundAndSetStepsToMove updatedCurrentRoundCells nextSteps roundNumber nextMapCell nextDirection mapCellDict notYetSetCoordinates

                else
                    goAroundAndSetStepsToMove currentRoundCells nextSteps roundNumber nextMapCell nextDirection mapCellDict notYetSetCoordinates


makeMapCellStatsForNextRound : GridDirection -> Int -> Int -> MapCoordinate -> ( MapCoordinate, Int, GridDirection )
makeMapCellStatsForNextRound currentDirection currentSteps roundNumber currentSpot =
    if currentSteps == roundNumber then
        let
            nextDirection =
                getNextDirectionForGoingAround currentDirection
        in
        ( getNextMapCoordinate nextDirection currentSpot, 1, nextDirection )

    else
        ( getNextMapCoordinate currentDirection currentSpot, currentSteps + 1, currentDirection )


addCurrentRoundCellsToMapCellDict : List ( Maybe Int, MapCell ) -> Dict String MapCell -> List MapCoordinate -> ( Dict String MapCell, List MapCoordinate )
addCurrentRoundCellsToMapCellDict currentRoundGridCells gridCellDict notYetSetCoordinates =
    -- We need to check cell before and after, maybe those have a lower step value.
    -- For this we add the last cell to the front and the first cell to the back.
    -- So our first cell is now the second, when we add it, we take the first three cells.
    -- The second is the one to add. We check if the first and third are next to the second, if they are, we check the steps for those.
    -- if its less then we add that value.
    let
        maybeFirstCell =
            List.head currentRoundGridCells

        maybeLastCell =
            List.head (List.reverse currentRoundGridCells)
    in
    case ( maybeFirstCell, maybeLastCell ) of
        ( Just firstCell, Just lastCell ) ->
            let
                adjustedCurrentRoundGridCells =
                    lastCell :: addToTheBackOfTheList firstCell currentRoundGridCells
            in
            handleCurrentRoundMapCellsRecursive adjustedCurrentRoundGridCells gridCellDict notYetSetCoordinates

        _ ->
            ( gridCellDict, notYetSetCoordinates )


handleCurrentRoundMapCellsRecursive : List ( Maybe Int, MapCell ) -> Dict String MapCell -> List MapCoordinate -> ( Dict String MapCell, List MapCoordinate )
handleCurrentRoundMapCellsRecursive currentRoundMapCells mapCells notYetSetCoordinates =
    let
        tupleResult =
            returnTupleOfFirstThreeElementsOfList currentRoundMapCells
    in
    case tupleResult of
        Err _ ->
            -- There are not anymore 3 cells in the last so we are done. We can ignore the error.
            ( mapCells, notYetSetCoordinates )

        Ok ( cellBefore, cellToAdd, cellAfter ) ->
            let
                currentRoundMapCellsForNextRound =
                    List.drop 1 currentRoundMapCells

                maybeCellBeforeComparedValue =
                    if areMapCoordinatesNextToEachOther (second cellBefore).mapCoordinate (second cellToAdd).mapCoordinate then
                        case first cellBefore of
                            Just beforeValue ->
                                case first cellToAdd of
                                    Nothing ->
                                        Just (beforeValue + 1)

                                    Just cellToAddValue ->
                                        Just (min cellToAddValue (beforeValue + 1))

                            Nothing ->
                                first cellToAdd

                    else
                        first cellToAdd

                finishedComparedValue =
                    if areMapCoordinatesNextToEachOther (second cellToAdd).mapCoordinate (second cellAfter).mapCoordinate then
                        case first cellAfter of
                            Just afterValue ->
                                case maybeCellBeforeComparedValue of
                                    Nothing ->
                                        Just (afterValue + 1)

                                    Just cellBeforeComparedValue ->
                                        Just (min cellBeforeComparedValue (afterValue + 1))

                            Nothing ->
                                maybeCellBeforeComparedValue

                    else
                        maybeCellBeforeComparedValue
            in
            case finishedComparedValue of
                Nothing ->
                    -- No cells around with a value, so this coordinate goes into our notYetSetCoordinates.
                    -- Then we continue with the rest of the list
                    let
                        updatedNotYetSetCoordinates =
                            (second cellToAdd).mapCoordinate :: notYetSetCoordinates
                    in
                    handleCurrentRoundMapCellsRecursive currentRoundMapCellsForNextRound mapCells updatedNotYetSetCoordinates

                Just stepsToSet ->
                    -- We found lowest steps, so we can set this cell in our dict
                    let
                        updatedMapCells =
                            setMapCellStepsInEmptyMapCell (stepsToSet + 1) (second cellToAdd).mapCoordinate mapCells
                    in
                    handleCurrentRoundMapCellsRecursive currentRoundMapCellsForNextRound updatedMapCells notYetSetCoordinates


handleNotYetSetCoordinates : List MapCoordinate -> Dict String MapCell -> Dict String MapCell
handleNotYetSetCoordinates notYetSetCoordinates mapCellDict =
    let
        totalCoordinates =
            List.length notYetSetCoordinates

        ( newNotYetSetCoordinates, newGridCellDict ) =
            List.foldr trySetStepsForCoordinate ( [], mapCellDict ) notYetSetCoordinates
    in
    if List.length newNotYetSetCoordinates == totalCoordinates then
        newGridCellDict

    else
        handleNotYetSetCoordinates newNotYetSetCoordinates newGridCellDict


trySetStepsForCoordinate : MapCoordinate -> ( List MapCoordinate, Dict String MapCell ) -> ( List MapCoordinate, Dict String MapCell )
trySetStepsForCoordinate mapCoordinate ( notYetSetCoordinates, mapCellDict ) =
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
        getLowestMovement (getNextMapCoordinate direction currentSpot) newLowest gridCellDict nextDirection


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
