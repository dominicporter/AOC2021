module Day5 exposing (..)

import Html


type alias Row =
    List Int


type alias Board =
    List Row


buildBoard : List Segment -> Board
buildBoard segments =
    let
        ( highestX, highestY ) =
            Debug.log "highests"
                (List.foldl
                    (\( ( startX, startY ), ( endX, endY ) ) ( accX, accY ) ->
                        ( max accX <| max startX endX, max accY <| max startY endY )
                    )
                    ( 0, 0 )
                    segments
                )
    in
    List.repeat (highestY + 1) (List.repeat (highestX + 1) 0)


moveAlongOne : Segment -> Segment
moveAlongOne ( ( startX, startY ), ( endX, endY ) ) =
    ( ( if startX < endX then
            startX + 1

        else
            startX
      , if startY < endY then
            startY + 1

        else
            startY
      )
    , ( if endX < startX then
            endX + 1

        else
            endX
      , if endY < startY then
            endY + 1

        else
            endY
      )
    )


incrementXY : Int -> Int -> Board -> Board
incrementXY x y board =
    List.indexedMap
        (\i row ->
            if i == y then
                List.indexedMap
                    (\j value ->
                        if j == x then
                            value + 1

                        else
                            value
                    )
                    row

            else
                row
        )
        board


incrementAtStart : Segment -> Board -> Board
incrementAtStart segment board =
    let
        ( ( startX, startY ), ( endX, endY ) ) =
            segment

        minX =
            Debug.log "minX" (min startX endX)

        minY =
            Debug.log "minY" (min startY endY)
    in
    incrementXY minX minY board


displayBoard : Board -> Board
displayBoard board =
    let
        nop =
            Debug.log "displayBoard" "displayBoard"

        nop1 =
            List.reverse board
            |> List.map
                (\row ->
                    Debug.log ""
                        (row
                            |> List.map String.fromInt
                            |> List.map
                                (\x ->
                                    if x == "0" then
                                        "."

                                    else
                                        x
                                )
                            |> String.concat
                        )
                )
                -- board
    in
    board


drawSegmentOnBoard : Segment -> Board -> Board
drawSegmentOnBoard segment board =
    let
        ( ( startX, startY ), ( endX, endY ) ) =
            Debug.log "segment" segment

        nop =
            displayBoard board
    in
    if startX == endX || startY == endY then
        if startX == endX && startY == endY then
            incrementXY startX startY board

        else
            drawSegmentOnBoard (moveAlongOne segment) (incrementAtStart segment board)

    else
        board


drawSegmentsOnBoard : Board -> List Segment -> Board
drawSegmentsOnBoard board segments =
    List.foldl
        drawSegmentOnBoard
        board
        segments


type alias Coord =
    ( Int, Int )


type alias Segment =
    ( Coord, Coord )


getOverLappingLinesCount : List Segment -> number
getOverLappingLinesCount segments =
    let
        startingBoard =
            buildBoard segments

        completedBoard =
            drawSegmentsOnBoard startingBoard segments
    in
        List.foldl
            (\row count1 ->
            List.foldl
                (\val count2 ->
                    if val > 1 then count2+1 else count2
                )
                count1
                row
            )
            0
            completedBoard


main : Html.Html msg
main =
    ""
        |> Html.text
