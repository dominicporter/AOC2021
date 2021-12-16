module Day6 exposing (..)

import Array exposing (..)
import Html
import Html.Attributes exposing (start)


breedOneDay : Array Int -> Array Int
breedOneDay startingSchool =
    let
        countBreeding = Maybe.withDefault 0 (get 0 startingSchool)
        countAtPos6 = Maybe.withDefault 0 (get 7 startingSchool)
    in
            startingSchool
            -- |> Debug.log "A" 
            |> slice 1 (length startingSchool)
            |> set 6 (countAtPos6+countBreeding)
            |> push countBreeding
            |> Debug.log "C" 

getLanternFishCount : Array Int -> Int -> Int
getLanternFishCount startingSchool numDays =
    if Debug.log "numDays" numDays == 0 then
        Array.foldl (+) 0 startingSchool

    else
        getLanternFishCount
            (breedOneDay startingSchool)
            (numDays - 1)


convertToCountMap : List Int -> Array Int
convertToCountMap startingSchool =
    List.foldl
        (\curr acc ->
            let
                currCount =
                    Maybe.withDefault 0 (Array.get curr acc)
            in
            Array.set curr (currCount + 1) acc
        )
        (Array.repeat 9 0)
        startingSchool


main : Html.Html msg
main =
    ""
        |> Html.text
