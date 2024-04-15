module Models.MainModel exposing (..)


type alias MainModel =
    { screenDimensions : ScreenDimensions }


startMainModel : MainModel
startMainModel =
    MainModel startScreenDimensions


type alias ScreenDimensions =
    { width : Float
    , height : Float
    }


startScreenDimensions : ScreenDimensions
startScreenDimensions =
    ScreenDimensions 0 0
