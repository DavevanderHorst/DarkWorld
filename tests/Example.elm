module Example exposing (..)

import Expect exposing (Expectation)
import Functions.Coordinates exposing (areMapCoordinatesNextToEachOther)
import Models.MainModel exposing (MapCoordinate)
import Test exposing (..)


suite : Test
suite =
    describe "check if map coordinates are next to each other"
        [ test "totally different coordinates" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 1) (MapCoordinate 100 100)
                    |> Expect.equal False
        , test "Same coordinate" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 1) (MapCoordinate 1 1)
                    |> Expect.equal False
        , test "Same row, next to each other" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 1) (MapCoordinate 2 1)
                    |> Expect.equal True
        , test "Same row, gap of one column" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 1) (MapCoordinate 3 1)
                    |> Expect.equal False
        , test "One row up, first coordinate is on even row, same column" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 2) (MapCoordinate 1 1)
                    |> Expect.equal True
        , test "One row up, one column further, first coordinate is on even row, " <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 2) (MapCoordinate 2 1)
                    |> Expect.equal True
        , test "One row up, two columns further, first coordinate is on even row, " <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 1 2) (MapCoordinate 3 1)
                    |> Expect.equal False
        , test "same as before, just coordinates switched " <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 3 1) (MapCoordinate 1 2)
                    |> Expect.equal False
        , test "final test 1" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 2 2) (MapCoordinate 2 3)
                    |> Expect.equal True
        , test "final test 2" <|
            \_ ->
                areMapCoordinatesNextToEachOther (MapCoordinate 2 2) (MapCoordinate 3 3)
                    |> Expect.equal True
        ]
