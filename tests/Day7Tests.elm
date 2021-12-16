module Day7Tests exposing (..)

import Array
import Day7 exposing (..)
import Day7TestData exposing (actualInput, sampleInput)
import Expect
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "Day7"
            [ describe "getMinFuelToAlignCrabs"
                [ test "works with sample input" <|
                    \_ ->
                        getMinFuelToAlignCrabs sampleInput
                            |> Expect.equal 37
                , test "works with actual input" <|
                    \_ ->
                        getMinFuelToAlignCrabs actualInput
                            |> Expect.equal 328187
                ]
            ]
