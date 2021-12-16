module Day5Tests exposing (..)

import Day5 exposing (..)
import Day5TestData exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day5" <|
        [ describe "drawSegmentsOnBoard"
            [ test "draws correctly on a small board" <|
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
        , 
            describe "moveAlongOne"
                [ test "works with a diagonal" <|
                    \_ ->
                        moveAlongOne ( ( 0, 0 ), ( 2, 2 ) )
                            |> moveAlongOne
                            |> Expect.equal ( ( 2, 2 ), ( 2, 2 ) )
                , test "reverse diagonal" <|
                    \_ ->
                        moveAlongOne ( ( 2, 2 ), ( 0, 0 ) )
                            |> moveAlongOne
                            |> Expect.equal ( ( 0, 0 ), ( 0, 0 ) )
                , test "reverse diagonal 1" <|
                    \_ ->
                        moveAlongOne ( ( 0, 2 ), ( 2, 0 ) )
                            |> moveAlongOne
                            |> Expect.equal ( ( 2, 0 ), ( 2, 0 ) )
                , test "horizontal" <|
                    \_ ->
                        moveAlongOne ( ( 5,10 ), ( 10, 10 ) )
                            |> moveAlongOne
                            |> Expect.equal ( ( 7, 10 ), ( 10, 10) )
                , test "vertical"  <|
                    \_ ->
                        moveAlongOne ( ( 5,20 ), ( 5, 10 ) )
                            |> moveAlongOne
                            |> Expect.equal ( ( 5, 18 ), ( 5, 10) )
                ]
        , describe "drawSegmentsOnBoardWithDiagonals"
            [ test "draws correctly on a small board" <|
                \_ ->
                    Expect.equal
                        (drawSegmentsOnBoardWithDiagonals
                            [ [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ]
                            [ ( ( 0, 0 ), ( 1, 1 ) ), ( ( 0, 0 ), ( 1, 1 ) ), ( ( 0, 1 ), ( 1, 0 ) ) ]
                        )
                        [ [ 2, 1 ], [ 1, 2 ], [ 0, 0 ] ]
            , test "draws correctly on a sample board" <|
                \_ ->
                    Expect.equal
                        (drawSegmentsOnBoardWithDiagonals
                            (buildBoard sampleSegments)
                            sampleSegments
                        )
                        sampleFinalBoardWithDiagonals
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
        , describe "getOverLappingLinesCount without Diagonals (Part 1)"
            [ test "works with Sample data" <|
                \_ ->
                    getOverLappingLinesCount sampleSegments False
                        |> Expect.equal 5

            -- This takes a long time...
            , skip <|
                test "works with actual data" <|
                    \_ ->
                        getOverLappingLinesCount actualSegments False
                            |> Expect.equal 5690
            ]
        , describe "getOverLappingLinesCount with Diagonals (Part 2)"
            [ test "works with Sample data" <|
                \_ ->
                    getOverLappingLinesCount sampleSegments True
                        |> Expect.equal 12

            -- This takes a long time...
            , 
                skip <| test "works with actual data" <|
                    \_ ->
                        getOverLappingLinesCount actualSegments True
                            |> Expect.equal 5690
            ]
        ]
