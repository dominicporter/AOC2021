module Day7 exposing (..)

import Array exposing (..)


tryToGetBetterThan bestSoFar posToMoveTo positionList =
    min
        bestSoFar
    <|
        List.foldl
            (\pos acc -> acc + abs (pos - posToMoveTo))
            0
            positionList


getMinFuelBetween : Int -> Int -> List Int -> Int -> Int
getMinFuelBetween minPos maxPos positionList bestSoFar =
    let
        foo =
            Debug.log "gmfb" [ minPos, maxPos, bestSoFar ]
    in
    if minPos == maxPos then
        tryToGetBetterThan bestSoFar maxPos positionList

    else
        let
            fuelForThisPos =
                tryToGetBetterThan bestSoFar maxPos positionList
        in
        getMinFuelBetween minPos (maxPos - 1) positionList fuelForThisPos


getMinFuelToAlignCrabs : List Int -> Int
getMinFuelToAlignCrabs positionList =
    let
        ( minPos, maxPos ) =
            Debug.log "min,max"
                (List.foldl
                    (\curr ( mi, mx ) ->
                        ( min curr mi, max curr mx )
                    )
                    ( 100000, -1 )
                    positionList
                )
    in
    getMinFuelBetween minPos maxPos positionList 100000000
