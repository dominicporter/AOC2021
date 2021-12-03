module Task1 exposing (countIncreaseWindows, main)

import Array exposing (foldl, fromList)
import Debug
import Html


countIncreaseWindows input =
    foldl
        (\curr acc ->
            case acc of
                [ nMin3, nMin2, nMin1, count ] ->
                    [ nMin2
                    , nMin1
                    , curr
                    , let
                        sumPrev3 =
                            nMin3 + nMin2 + nMin1

                        sumCurr3 =
                            nMin2 + nMin1 + curr
                      in
                      if nMin3 > 0 && nMin2 > 0 && nMin1 > 0 && sumCurr3 > sumPrev3 then
                        count + 1

                      else
                        count
                    ]

                _ ->
                    Debug.log "count" [ 0, 0, 0, 0 ]
        )
        [ 0, 0, 0, 0 ]
        (fromList input)
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0


main : Html.Html msg
main =
    countIncreaseWindows []
        |> String.fromInt
        |> Html.text
