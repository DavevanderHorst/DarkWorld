module Main exposing (init, main, update)

import Browser
import Browser.Dom
import Functions.Dict.Get exposing (tryGetMapCellFromMapCellDict)
import Functions.Dict.Insert exposing (insertMapCellInDictUnSafe)
import Functions.Hover exposing (trySetHoverColorInTempMapCells)
import Functions.ToString exposing (cellContentToString, mapCoordinateToString)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel, Map, ScreenDimensions)
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

        MapCellIsHovered mapCoordinate ->
            -- hovering is only on out of combat
            -- only empty cells, which can moved too, can be hovered
            -- so this means there must a possible path towards hovered cell.
            -- we set this path in our temp map, so that it can be instantly removed.
            let
                updatedMapResult =
                    trySetHoverColorInTempMapCells mapCoordinate model.currentMap
            in
            case updatedMapResult of
                Err error ->
                    ( { model | error = Just error }, Cmd.none )

                Ok updatedMap ->
                    ( { model | currentMap = updatedMap }, Cmd.none )

        MapCellIsLeft ->
            let
                updatedMap =
                    removeTempMapCellsFromCurrentMap model.currentMap
            in
            ( { model | currentMap = updatedMap }, Cmd.none )

        MapCellIsClicked clickedMapCoordinate ->
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


removeTempMapCellsFromCurrentMap : Map -> Map
removeTempMapCellsFromCurrentMap map =
    { map | tempMapCells = Nothing }


subscriptions : MainModel -> Sub Msg
subscriptions _ =
    Sub.none
