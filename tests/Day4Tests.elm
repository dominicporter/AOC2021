module Day4Tests exposing (..)

import Day4 exposing (..)
import Day4TestData exposing (actualBingoCards, actualCalledNumbers)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day4" <|
        let
            sampleCalledNumbers =
                [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1 ]

            sampleBingoCards : List Board
            sampleBingoCards =
                [ [ [ 22, 13, 17, 11, 0 ]
                  , [ 8, 2, 23, 4, 24 ]
                  , [ 21, 9, 14, 16, 7 ]
                  , [ 6, 10, 3, 18, 5 ]
                  , [ 1, 12, 20, 15, 19 ]
                  ]
                , [ [ 3, 15, 0, 2, 22 ]
                  , [ 9, 18, 13, 17, 5 ]
                  , [ 19, 8, 7, 25, 23 ]
                  , [ 20, 11, 10, 24, 4 ]
                  , [ 14, 21, 16, 12, 6 ]
                  ]
                , [ [ 14, 21, 17, 24, 4 ]
                  , [ 10, 16, 15, 9, 19 ]
                  , [ 18, 8, 23, 26, 20 ]
                  , [ 22, 11, 13, 6, 5 ]
                  , [ 2, 0, 12, 3, 7 ]
                  ]
                ]
        in
        [ describe "getMaybeWinningBoard"
            [ test "Finds a board with a winning row" <|
                \_ ->
                    Expect.equal
                        (getMaybeWinningBoard
                            [ [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              , [ ( 4, True ), ( 5, True ), ( 6, True ) ]
                              ]
                            , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                              , [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              ]
                            ]
                        )
                    <|
                        Just
                            [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                            , [ ( 4, True ), ( 5, True ), ( 6, True ) ]
                            ]
            , test "Finds a board with a winning column" <|
                \_ ->
                    Expect.equal
                        (getMaybeWinningBoard
                            [ [ [ ( 1, False ), ( 2, True ), ( 3, False ) ]
                              , [ ( 4, False ), ( 5, True ), ( 6, False ) ]
                              ]
                            , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                              , [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              ]
                            ]
                        )
                    <|
                        Just
                            [ [ ( 1, False ), ( 2, True ), ( 3, False ) ]
                            , [ ( 4, False ), ( 5, True ), ( 6, False ) ]
                            ]
            , test "Returns Nothing if no winning Board" <|
                \_ ->
                    Expect.equal
                        (getMaybeWinningBoard
                            [ [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              , [ ( 4, True ), ( 5, True ), ( 6, False ) ]
                              ]
                            , [ [ ( 7, False ), ( 8, True ), ( 9, False ) ]
                              , [ ( 1, False ), ( 2, False ), ( 3, True ) ]
                              ]
                            ]
                        )
                        Nothing
            ]
        , describe "markBoards"
            [ test "works with simple boards" <|
                \_ ->
                    Expect.equal
                        (markBoards 3
                            [ [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              , [ ( 4, False ), ( 5, False ), ( 6, False ) ]
                              ]
                            , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                              , [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              ]
                            ]
                        )
                        [ [ [ ( 1, False ), ( 2, False ), ( 3, True ) ]
                          , [ ( 4, False ), ( 5, False ), ( 6, False ) ]
                          ]
                        , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                          , [ ( 1, False ), ( 2, False ), ( 3, True ) ]
                          ]
                        ]
            ]
        , describe "getWinningBoard"
            [ test "works with simple boards" <|
                \_ ->
                    Expect.equal
                        (getFirstWinningBoard [ 3, 1, 2 ]
                            [ [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                              , [ ( 4, False ), ( 5, False ), ( 6, False ) ]
                              ]
                            , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                              , [ ( 1, False ), ( 2, False ), ( 4, False ) ]
                              ]
                            ]
                        )
                        ( [ [ ( 1, True ), ( 2, True ), ( 3, True ) ]
                          , [ ( 4, False ), ( 5, False ), ( 6, False ) ]
                          ]
                        , 2
                        )
            ]
        , describe "getMarkableBoards"
            [ test "works with simple boards" <|
                \_ ->
                    Expect.equal (getMarkableBoards [ [ [ 1, 2, 3 ], [ 4, 5, 6 ] ], [ [ 7, 8, 9 ], [ 1, 2, 3 ] ] ])
                        [ [ [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                          , [ ( 4, False ), ( 5, False ), ( 6, False ) ]
                          ]
                        , [ [ ( 7, False ), ( 8, False ), ( 9, False ) ]
                          , [ ( 1, False ), ( 2, False ), ( 3, False ) ]
                          ]
                        ]
            ]
        , describe "getBingoFinalScore"
            [ test "works with sample" <|
                \_ ->
                    Expect.equal (getBingoFinalScore sampleCalledNumbers sampleBingoCards) 4512
            , test "works with actual Input" <|
                \_ ->
                    Expect.equal (getBingoFinalScore actualCalledNumbers actualBingoCards) 6592
            ]
        , describe "getBingoFinalScoreForLastWinningBoard"
            [ test "works with sample" <|
                \_ ->
                    Expect.equal (getBingoFinalScoreForLastWinningBoard sampleCalledNumbers sampleBingoCards) 1924
            , test "works with actual Input" <|
                \_ ->
                    Expect.equal (getBingoFinalScoreForLastWinningBoard actualCalledNumbers actualBingoCards) 31755
            ]
        ]
