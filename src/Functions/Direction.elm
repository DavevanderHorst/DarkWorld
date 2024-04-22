module Functions.Direction exposing (..)

import Models.Types exposing (GridDirection(..))


getNextDirectionForGoingAround : GridDirection -> GridDirection
getNextDirectionForGoingAround direction =
    -- for circling a mapCell
    case direction of
        Left ->
            UpLeft

        UpLeft ->
            UpRight

        UpRight ->
            Right

        Right ->
            DownRight

        DownRight ->
            DownLeft

        DownLeft ->
            Left
