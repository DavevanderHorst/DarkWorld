module Models.StartModels exposing (..)

import Dict
import Maps.StartMap exposing (makeStartMap, startHeroSpot)
import Models.MainModel exposing (MainModel, Map, MapCoordinate, ScreenDimensions)
import Models.Types exposing (AnimationType(..), CellMovementState(..))


startMainModel : MainModel
startMainModel =
    let
        startMapResult =
            makeStartMap

        basicMainModel =
            MainModel emptyScreenDimensions emptyMap startHeroSpot NoAnimation Nothing
    in
    case startMapResult of
        Ok startMap ->
            { basicMainModel | currentMap = startMap }

        Err error ->
            { basicMainModel | error = Just error }


emptyScreenDimensions : ScreenDimensions
emptyScreenDimensions =
    ScreenDimensions 0 0


emptyMap : Map
emptyMap =
    Map 1 Dict.empty Active
