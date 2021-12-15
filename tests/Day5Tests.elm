module Day5Tests exposing (..)

import Day5 exposing (..)
import Day5TestData exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day5" <|
        [ describe "drawSegmentsOnBoard"
            [ test "draws correcly on a small board" <|
                \_ ->
                    Expect.equal
                        (drawSegmentsOnBoard
                            [ [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ]
                            [ ( ( 0, 2 ), ( 1, 2 ) ) ]
                        )
                        [ [ 0, 0 ], [ 0, 0 ], [ 1, 1 ] ]
            , test "draws correctly on a sample board" <|
                \_ ->
                    Expect.equal
                        (drawSegmentsOnBoard
                            (buildBoard sampleSegments)
                            sampleSegments
                        )
                        sampleFinalBoard
            ]
        , describe "buildBoard"
            [ test "builds a small board" <|
                \_ ->
                    Expect.equal
                        (buildBoard
                            [ ( ( 0, 2 ), ( 1, 2 ) ) ]
                        )
                        [ [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ]
            , test "builds a full board" <|
                \_ ->
                    Expect.equal (List.length <| buildBoard actualSegments) 991
            ]
        , describe "getOverLappingLinesCount"
            [ test "works with Sample data" <|
                \_ ->
                    Expect.equal (getOverLappingLinesCount sampleSegments) 5

            -- This takes a long time...
            , skip <|
                test "works with actual data" <|
                    \_ ->
                        Expect.equal (getOverLappingLinesCount actualSegments) 5690
            ]
        ]
