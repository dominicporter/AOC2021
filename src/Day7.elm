module Day7 exposing (..)

-- import Array exposing (..)


getComplexFuel fuelUsed =
    List.range 1 fuelUsed
        |> List.foldl (+) 0


tryToGetBetterThan : Int -> Int -> List Int -> Int
tryToGetBetterThan bestSoFar posToMoveTo positionList =
    min
        bestSoFar
    <|
        List.foldl
            (\pos acc ->
                let
                    fuelUsed =
                        abs (pos - posToMoveTo)

                    complexFuel =
                        getComplexFuel fuelUsed
                in
                acc + complexFuel
            )
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
