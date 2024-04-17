module Models.StartModels exposing (..)

import Maps.StartMap exposing (makeStartMap)
import Models.MainModel exposing (MainModel, ScreenDimensions)


startMainModel : MainModel
startMainModel =
    MainModel startScreenDimensions Nothing makeStartMap


startScreenDimensions : ScreenDimensions
startScreenDimensions =
    ScreenDimensions 0 0
