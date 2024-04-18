module Models.StartModels exposing (..)

import Dict
import Maps.StartMap exposing (makeStartMap)
import Models.MainModel exposing (MainModel, Map, ScreenDimensions)


startMainModel : MainModel
startMainModel =
    let
        startMapResult =
            makeStartMap
    in
    case startMapResult of
        Ok startMap ->
            MainModel startScreenDimensions Nothing startMap

        Err error ->
            MainModel startScreenDimensions (Just error) emptyMap


startScreenDimensions : ScreenDimensions
startScreenDimensions =
    ScreenDimensions 0 0


emptyMap : Map
emptyMap =
    Map 1 Dict.empty
