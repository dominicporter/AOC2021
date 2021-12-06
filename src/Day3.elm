module Day3 exposing (addToTotals, getCountsForBinaryString, getGammaAndEpsilon, getPowerConsumption, convertCountListToInt, main)

import Array exposing (foldl, fromList)
import Debug
import Html


getCountsForBinaryString : String -> List Int
getCountsForBinaryString binString =
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


addToTotals : List Int -> List Int -> List Int
addToTotals list1 list2 =
    if List.length list1 > 0 then
        Maybe.withDefault 0 (List.head list1)
            + Maybe.withDefault 0 (List.head list2)
            :: addToTotals (Maybe.withDefault [] (List.tail list1)) (Maybe.withDefault [] (List.tail list2))

    else
        []

getGammaCountList input = 
    Debug.log "countList" <|
                foldl
                    (\curr acc ->
                        let
                            currCount =
                                getCountsForBinaryString curr
                        in
                        addToTotals currCount acc
                    )
                    -- List of zeros [0,0,...]
                    (List.repeat (String.length (Maybe.withDefault "" <| List.head input)) 0)
                    (fromList input)

convertCountListToInt : List Int -> Int -> Bool -> Int
convertCountListToInt countList inputCount gamma  = 
    if List.length countList == 0
    then 
        0
    else
        let
            bitPos = (List.length countList)
            leftMost = (Maybe.withDefault 0 <| List.head countList)
            _ = Debug.log "countList" (countList, leftMost, bitPos)
            theRest = Debug.log "theRest" (convertCountListToInt (Maybe.withDefault [] <| List.tail countList) inputCount gamma)
        in 
        Debug.log "result" <|if leftMost > inputCount // 2 
                then
                    if gamma then theRest + 2^(bitPos - 1) else theRest
                else
                    if gamma then theRest else theRest + 2^(bitPos - 1)
    

getGammaAndEpsilon : List String -> ( Int, Int )
getGammaAndEpsilon input =
    let
        inputCount = (List.length input)
        -- gammaCountList looks like [1,5,3,0,5]
        gammaCountList = getGammaCountList input
        gamma = convertCountListToInt gammaCountList inputCount True

        epsilon = convertCountListToInt gammaCountList inputCount False
    in
    ( gamma, epsilon )


getPowerConsumption input =
    let
        ( gamma, epsilon ) =
            getGammaAndEpsilon input
    in
    gamma * epsilon


main : Html.Html msg
main =
    getPowerConsumption []
        |> String.fromInt
        |> Html.text
