module Maps.MapSizes exposing (..)


mapCellSquareSize : Int
mapCellSquareSize =
    -- warning must be divisible by 2 and 4
    -- 2 because of the horizontal shift
    -- 4 because of the vertical shift up, 25%
    52


mapCellTotalRows : Int
mapCellTotalRows =
    -- uneven looks nicer
    15


mapCellUnevenRowTotalColumns : Int
mapCellUnevenRowTotalColumns =
    19


roomBetweenHorizontalCells : Int
roomBetweenHorizontalCells =
    -- warning must be divisible by 2
    4


blackBackgroundMapMargin : Int
blackBackgroundMapMargin =
    30


mapWidth : Int
mapWidth =
    -- total width = number of horizontal cells + space in between + blackMapMargin on both sides
    (mapCellUnevenRowTotalColumns * mapCellTotalHorizontalWidth) - roomBetweenHorizontalCells + (blackBackgroundMapMargin * 2)


mapWidthInPxString : String
mapWidthInPxString =
    makePixelStringFromInt mapWidth


mapHeight : Int
mapHeight =
    -- total rows * vertical height + background margin - 1 * shift
    mapCellTotalRows * mapCellTotalVerticalHeight + (blackBackgroundMapMargin * 2) + quarterMapCellSquareSize


mapHeightInPxString : String
mapHeightInPxString =
    makePixelStringFromInt mapHeight


mapCellEvenRowTotalColumns : Int
mapCellEvenRowTotalColumns =
    mapCellUnevenRowTotalColumns - 1


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
