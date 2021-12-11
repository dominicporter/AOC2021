module Day3 exposing (addListsOfNumbers, convertBinStringToInt, convertBinStringToListOfBitDigits, convertCountListToInt, getGammaAndEpsilon, getLifeSupportRating, getNumWithMostCommonStartBits, getPowerConsumption, main)

import Array exposing (Array, foldl, fromList, length)
import Debug
import Html
import Task1 exposing (countIncreaseWindows)


convertBinStringToListOfBitDigits : String -> List Int
convertBinStringToListOfBitDigits binString =
    String.foldl
        (\curr acc ->
            case curr of
                '1' ->
                    acc ++ [ 1 ]

                _ ->
                    acc ++ [ 0 ]
        )
        []
        binString


addListsOfNumbers : List Int -> List Int -> List Int
addListsOfNumbers list1 list2 =
    if List.length list1 > 0 then
        Maybe.withDefault 0 (List.head list1)
            + Maybe.withDefault 0 (List.head list2)
            :: addListsOfNumbers (Maybe.withDefault [] (List.tail list1)) (Maybe.withDefault [] (List.tail list2))

    else
        []


getCountOfOnesList : List String -> List Int
getCountOfOnesList input =
    Debug.log "countList" <|
        foldl
            (\curr acc ->
                let
                    currCount =
                        convertBinStringToListOfBitDigits curr
                in
                addListsOfNumbers currCount acc
            )
            -- List of zeros [0,0,...]
            (List.repeat (String.length (Maybe.withDefault "" <| List.head input)) 0)
            (fromList input)


convertCountListToInt : List Int -> Int -> Bool -> Int
convertCountListToInt countList inputCount gamma =
    if List.length countList == 0 then
        0

    else
        let
            bitPos =
                List.length countList

            leftMost =
                Maybe.withDefault 0 <| List.head countList

            _ =
                ( countList, leftMost, bitPos )

            theRest =
                convertCountListToInt (Maybe.withDefault [] <| List.tail countList) inputCount gamma
        in
        Debug.log "result" <|
            if leftMost > inputCount // 2 then
                if gamma then
                    theRest + 2 ^ (bitPos - 1)

                else
                    theRest

            else if gamma then
                theRest

            else
                theRest + 2 ^ (bitPos - 1)


getGammaAndEpsilon : List String -> ( Int, Int )
getGammaAndEpsilon input =
    let
        inputCount =
            List.length input

        -- gammaCountList looks like [1,5,3,0,5]
        gammaCountList =
            getCountOfOnesList input

        gamma =
            convertCountListToInt gammaCountList inputCount True

        epsilon =
            convertCountListToInt gammaCountList inputCount False
    in
    ( gamma, epsilon )


getPowerConsumption : List String -> Int
getPowerConsumption input =
    let
        ( gamma, epsilon ) =
            getGammaAndEpsilon input
    in
    gamma * epsilon


convertBinStringToInt : String -> Int
convertBinStringToInt bitString =
    if String.length bitString == 0 then
        0

    else
        let
            bitPos =
                String.length bitString

            leftMost =
                String.left 1 bitString

            _ =
                ( bitString, leftMost, bitPos )

            theRest =
                convertBinStringToInt (String.dropLeft 1 bitString)
        in
        if leftMost == "1" then
            theRest + 2 ^ (bitPos - 1)

        else
            theRest


getMostCommonBitAtPosition : List String -> Int -> Char
getMostCommonBitAtPosition input position =
    let
        countOfOnes =
            Debug.log "" <|
                foldl
                    (\curr acc ->
                        if (String.toList curr |> Array.fromList |> Array.get position |> Maybe.withDefault '0') == '1' then
                            acc + 1

                        else
                            acc
                    )
                    0
                    (fromList input)
    in
    if countOfOnes >= (List.length input - countOfOnes) then
        '1'

    else
        '0'


getLeastCommonBitAtPosition input position =
    let
        countOfOnes =
            Debug.log "countOfOnes" <|
                foldl
                    (\curr acc ->
                        if (String.toList curr |> Array.fromList |> Array.get position |> Maybe.withDefault '0') == '1' then
                            acc + 1

                        else
                            acc
                    )
                    0
                    (fromList input)
    in
    if countOfOnes >= (List.length input - countOfOnes) then
        '0'

    else
        '1'


getAllWithBitAtPosition : List String -> Int -> Char -> List String
getAllWithBitAtPosition input position mostCommonBitAtPosition =
    List.filter (\el -> (String.toList el |> Array.fromList |> Array.get position |> Maybe.withDefault '1') == mostCommonBitAtPosition) input


getNumWithMostCommonStartBits : List String -> Int -> Int
getNumWithMostCommonStartBits input position =
    let
        nop =
            Debug.log "getNumWithMostCommonStartBits" ( input, position )

        mostCommonBitAtPosition =
            Debug.log "mostCommonBitAtPosition" (getMostCommonBitAtPosition input position)

        filteredList =
            Debug.log "filteredList" (getAllWithBitAtPosition input position mostCommonBitAtPosition)
    in
    if List.length filteredList <= 1 then
        Maybe.withDefault "" (List.head filteredList)
            |> convertBinStringToInt

    else
        getNumWithMostCommonStartBits filteredList (position + 1)


getNumWithLeastCommonStartBits : List String -> Int -> Int
getNumWithLeastCommonStartBits input position =
    let
        nop =
            Debug.log "getNumWithLeastCommonStartBits" ( input, position )

        leastCommonBitAtPosition =
            Debug.log "leastCommonBitAtPosition" (getLeastCommonBitAtPosition input position)

        filteredList =
            Debug.log "filteredList" (getAllWithBitAtPosition input position leastCommonBitAtPosition)
    in
    if List.length filteredList <= 1 then
        Maybe.withDefault "" (List.head filteredList)
            |> convertBinStringToInt

    else
        getNumWithLeastCommonStartBits filteredList (position + 1)


getOgrAndCsr : List String -> ( Int, Int )
getOgrAndCsr input =
    let
        ogr =
            getNumWithMostCommonStartBits input 0

        csr =
            getNumWithLeastCommonStartBits input 0
    in
    ( ogr, csr )


getLifeSupportRating : List String -> Int
getLifeSupportRating input =
    let
        ( ogr, csr ) =
            getOgrAndCsr input
    in
    ogr * csr


main : Html.Html msg
main =
    getPowerConsumption []
        |> String.fromInt
        |> Html.text
