module Day6Tests exposing (..)

import Array
import Day6 exposing (..)
import Day6TestData exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "Day6" <|
            [ describe "convertToCountMap"
                [ test "works with sampleInput" <|
                    \_ ->
                        convertToCountMap sampleInput
                            |> Array.toList
                            |> Expect.equal [ 0, 1, 1, 2, 1, 0, 0, 0, 0 ]
                ]
            , describe "getLanternFishCount"
                [ test "works with sample over 2 days" <|
                    \_ ->
                        getLanternFishCount (convertToCountMap sampleInput) 2
                            |> Expect.equal 6
                , test "works with sample over 18 days" <|
                    \_ ->
                        getLanternFishCount (convertToCountMap sampleInput) 18
                            |> Expect.equal 26
                , test "works with sample over 80 days" <|
                    \_ ->
                        getLanternFishCount (convertToCountMap sampleInput) 80
                            |> Expect.equal 5934
                , test "works with actual input over 80 days" <|
                    \_ ->
                        getLanternFishCount (convertToCountMap actualInput) 80
                            |> Expect.equal 346063
                , test "works with actual input over 256 days" <|
                    \_ ->
                        getLanternFishCount (convertToCountMap actualInput) 256
                            |> Expect.equal 1572358335990
                ]
            ]
