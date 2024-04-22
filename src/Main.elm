module Main exposing (init, main, update)

import Browser
import Browser.Dom
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe)
import Functions.MoveAnimation exposing (tryMakeMoveAnimation)
import Functions.Movement exposing (trySetMovementAroundHeroInMapCells)
import Functions.ToString exposing (cellContentToString, mapCoordinateToString)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel, ScreenDimensions)
import Models.StartModels exposing (startMainModel)
import Models.Types exposing (CellMovementState(..), MapCellContent(..))
import Task
import View.MainView exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( MainModel, Cmd Msg )
init _ =
    ( startMainModel
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            let
                newScreenDimensions =
                    ScreenDimensions viewPort.viewport.width viewPort.viewport.height
            in
            ( { model | screenDimensions = newScreenDimensions }, Cmd.none )

        MapIsClicked clickedMapCoordinate ->
            let
                currentMap =
                    model.currentMap

                currentHeroMapCellResult =
                    tryGetMapCellFromMapCellDict model.heroSpotOnCurrentMap currentMap.mapCells
            in
            case currentHeroMapCellResult of
                Err error ->
                    ( { model | error = Just error }, Cmd.none )

                Ok currentHeroMapCell ->
                    if currentHeroMapCell.content /= Hero then
                        let
                            newError =
                                { method = "MapIsClicked"
                                , error =
                                    "Saved hero spot in model = "
                                        ++ mapCoordinateToString model.heroSpotOnCurrentMap
                                        ++ ". But this cell is "
                                        ++ cellContentToString currentHeroMapCell.content
                                }
                        in
                        ( { model | error = Just newError }, Cmd.none )

                    else
                        let
                            oldHeroMapCell =
                                { currentHeroMapCell | content = Empty }

                            mapCellsWithoutHero =
                                insertMapCellInDictUnSafe oldHeroMapCell currentMap.mapCells

                            updatedMap =
                                -- We remove hero from map, and make sure nothing can be clicked while move animation is active.
                                { currentMap | mapCells = mapCellsWithoutHero, cellMovementState = Passive }
                        in
                        ( { model | currentMap = updatedMap }, Cmd.none )


subscriptions : MainModel -> Sub Msg
subscriptions _ =
    Sub.none
