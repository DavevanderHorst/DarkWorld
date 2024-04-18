module Models.StartModels exposing (..)

import Dict
import Maps.StartMap exposing (makeStartMap, startHeroSpot)
import Models.MainModel exposing (MainModel, Map, MapCoordinate, ScreenDimensions)


startMainModel : MainModel
startMainModel =
    let
        startMapResult =
            makeStartMap
    in
    case startMapResult of
        Ok startMap ->
            MainModel emptyScreenDimensions Nothing startMap startHeroSpot

        Err error ->
            MainModel emptyScreenDimensions (Just error) emptyMap startHeroSpot


emptyScreenDimensions : ScreenDimensions
emptyScreenDimensions =
    ScreenDimensions 0 0


emptyMap : Map
emptyMap =
    Map 1 Dict.empty
