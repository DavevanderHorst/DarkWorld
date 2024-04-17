module Models.MainModel exposing (..)

import Dict exposing (Dict)


type alias MainModel =
    { screenDimensions : ScreenDimensions
    , error : Maybe Error
    , currentMap : Map
    }


type alias ScreenDimensions =
    { width : Float
    , height : Float
    }


type alias Map =
    { mapNumber : Int
    , mapCells : Dict String MapCell
    }


type alias MapCell =
    { startWidthInPx : String
    , startHeightInPx : String
    , mapCoordinate : MapCoordinate
    }


type alias MapCoordinate =
    { columnNumber : Int
    , rowNumber : Int
    }


type alias Error =
    { method : String
    , error : String
    }
