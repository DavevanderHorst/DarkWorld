module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Calculate amount of rounds needed to hit all cells of the map"
        [ test "small map, hero at first point" <|
            \_ ->
                1
                    + 1
                    |> Expect.equal 2
        , test "medium map, hero at first point" <|
            \_ ->
                2
                    + 5
                    |> Expect.equal 7
        ]
