module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Models.MainModel exposing (MapCoordinate)


type Msg
    = GotViewport Viewport
    | MapCellIsClicked MapCoordinate
    | MapCellIsHovered MapCoordinate
    | MapCellIsLeft
