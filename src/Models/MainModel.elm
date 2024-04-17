module Models.MainModel exposing (..)


type alias MainModel =
    { screenDimensions : ScreenDimensions
    , error : Maybe Error
    }


startMainModel : MainModel
startMainModel =
    MainModel startScreenDimensions Nothing


type alias ScreenDimensions =
    { width : Float
    , height : Float
    }


startScreenDimensions : ScreenDimensions
startScreenDimensions =
    ScreenDimensions 0 0


type alias Error =
    { method : String
    , error : String
    }
