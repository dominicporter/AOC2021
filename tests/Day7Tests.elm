module Day7Tests exposing (..)

import Array
import Day7 exposing (..)
import Day7TestData exposing (actualInput, sampleInput)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day7"
        [ describe "getMinFuelToAlignCrabs"
            [ skip <|
                test "works with sample input" <|
                    \_ ->
                        getMinFuelToAlignCrabs sampleInput
                            |> Expect.equal 37
            , test "works with sample input and complex fuel calcs" <|
                \_ ->
                    getMinFuelToAlignCrabs sampleInput
                        |> Expect.equal 168
            , skip <|
                test "works with actual input" <|
                    \_ ->
                        getMinFuelToAlignCrabs actualInput
                            |> Expect.equal 328187

            -- this is slow
            , skip <|
                test "works with actual input and complex fuel calcs" <|
                    \_ ->
                        getMinFuelToAlignCrabs actualInput
                            |> Expect.equal 91257582
            ]
        , describe "getComplexFuel"
            [ test "works with simple nums" <|
                \_ ->
                    getComplexFuel 3
                        |> Expect.equal 6
            ]
        ]
