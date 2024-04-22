module Models.Types exposing (..)

import Models.Point exposing (Point)


type MapCellContent
    = Empty
    | Hero
    | Monster MonsterType


type MonsterType
    = Dummy


type CellMovementState
    = Active
    | Passive


type AnimationType
    = NoAnimation
    | AnimationMove Point (List Point)


type GridDirection
    = Left
    | UpLeft
    | UpRight
    | Right
    | DownRight
    | DownLeft
