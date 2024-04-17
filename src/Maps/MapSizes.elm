module Maps.MapSizes exposing (..)


mapWidth : Int
mapWidth =
    -- total width = number of horizontal cells + space in between
    (mapCellUnevenRowTotalColumns * mapCellTotalHorizontalWidth) - roomBetweenHorizontalCells


mapWidthInPxString : String
mapWidthInPxString =
    makePixelStringFromInt mapWidth


mapHeight : Int
mapHeight =
    mapCellTotalRows * mapCellTotalVerticalHeight


mapHeightInPxString : String
mapHeightInPxString =
    makePixelStringFromInt mapHeight


mapCellTotalRows : Int
mapCellTotalRows =
    -- uneven looks nicer
    9


mapCellUnevenRowTotalColumns : Int
mapCellUnevenRowTotalColumns =
    15


mapCellEvenRowTotalColumns : Int
mapCellEvenRowTotalColumns =
    mapCellUnevenRowTotalColumns - 1


mapCellSquareSize : Int
mapCellSquareSize =
    -- warning must be divisible by 2 and 4
    -- 2 because of the horizontal shift
    -- 4 because of the vertical shift up, 25%
    40


mapCellSquareSizeInPixelString : String
mapCellSquareSizeInPixelString =
    makePixelStringFromInt mapCellSquareSize


mapCellTotalHorizontalWidth : Int
mapCellTotalHorizontalWidth =
    mapCellSquareSize + roomBetweenHorizontalCells


mapCellTotalVerticalHeight : Int
mapCellTotalVerticalHeight =
    mapCellSquareSize + roomBetweenVerticalCells - quarterMapCellSquareSize


halfMapCellSquareSize : Int
halfMapCellSquareSize =
    mapCellSquareSize // 2


quarterMapCellSquareSize : Int
quarterMapCellSquareSize =
    mapCellSquareSize // 4


roomBetweenHorizontalCells : Int
roomBetweenHorizontalCells =
    -- warning must be divisible by 2
    4


halfRoomBetweenHorizontalCells : Int
halfRoomBetweenHorizontalCells =
    roomBetweenHorizontalCells // 2


roomBetweenVerticalCells : Int
roomBetweenVerticalCells =
    roomBetweenHorizontalCells // 2


evenRowHorizontalBaseShift : Int
evenRowHorizontalBaseShift =
    -- half the size of a map cell + half the size whats between horizontal map cells
    halfMapCellSquareSize + halfRoomBetweenHorizontalCells


makePixelStringFromInt : Int -> String
makePixelStringFromInt int =
    String.fromInt int ++ "px"
