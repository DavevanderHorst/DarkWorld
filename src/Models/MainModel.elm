module Models.MainModel exposing (..)

import Dict exposing (Dict)
import Models.Types exposing (AnimationType, CellMovementState, Color, MapCellContent)


type alias MainModel =
    { screenDimensions : ScreenDimensions
    , currentMap : Map
    , heroSpotOnCurrentMap : MapCoordinate
    , animation : AnimationType
    , error : Maybe Error
    }


type alias ScreenDimensions =
    { width : Float
    , height : Float
    }


type alias Map =
    { mapNumber : Int

    -- this is our map
    , mapCells : Dict String MapCell

    -- our temp map, in where we can put fast changes, like temporary paths
    -- temp map is rendered first, but if not exists then normal map is rendered.
    -- this makes it easy and fast to for example remove temporary paths
    , tempMapCells : Maybe (Dict String MapCell)

    -- this is or if cell are activated for moving, when there is an animation or combat they are deactivated
    , cellMovementState : CellMovementState
    }


type alias MapCell =
    { startWidth : Int
    , startHeight : Int
    , mapCoordinate : MapCoordinate
    , content : MapCellContent
    , stepsToMoveTowards : Maybe Int
    , cellColor : Color
    }


type alias MapCoordinate =
    { columnNumber : Int
    , rowNumber : Int
    }


type alias Error =
    { -- The method in which the error occurred
      method : String

    -- the error explained in details
    , error : String
    }
