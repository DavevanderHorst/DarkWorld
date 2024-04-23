module View.Colors exposing (..)

import Models.Types exposing (Color(..))


whiteColorString : String
whiteColorString =
    "white"


heroMapCellColor : String
heroMapCellColor =
    "lemonchiffon"


monsterAgroColorString : String
monsterAgroColorString =
    "#FFE0DF"


getHtmlColor : Color -> String
getHtmlColor color =
    case color of
        EmptyColor ->
            whiteColorString

        HoveredColor ->
            "red"

        PartOfPathColor ->
            "yellow"
