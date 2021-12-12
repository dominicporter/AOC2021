module Day4 exposing (..)

import Array
import Html


type alias Row =
    List Int


type alias Board =
    List Row


type alias MarkableBoard =
    List MarkableRow


type alias MarkableRow =
    List ( Int, Bool )


getMarkableBoards : List Board -> List MarkableBoard
getMarkableBoards boards =
    List.map
        (\board ->
            List.map
                (\row ->
                    List.map (\num -> ( num, False ))
                        row
                )
                board
        )
        boards


markBoards : Int -> List MarkableBoard -> List MarkableBoard
markBoards numberCalled boards =
    List.map
        (\board ->
            List.map
                (\row ->
                    List.map
                        (\( num, state ) ->
                            ( num
                            , if num == numberCalled then
                                True

                              else
                                state
                            )
                        )
                        row
                )
                board
        )
        boards


isWinningBoard : MarkableBoard -> Bool
isWinningBoard board =
    let
        winningRows =
            List.map
                (\row ->
                    List.all (\( _, marked ) -> marked == True)
                        row
                )
                board

        winningColumns =
            List.foldl
                (\row acc ->
                    List.map
                        (\( index, isWinningColumn ) ->
                            (row
                                |> Array.fromList
                                |> Array.get index
                                |> Maybe.withDefault ( 0, False )
                                |> Tuple.second
                            )
                                && isWinningColumn
                        )
                        (List.indexedMap Tuple.pair acc)
                )
                -- [True, True, ...]
                (List.repeat (List.length <| Maybe.withDefault [] <| List.head board) True)
                board
    in
    List.any (\r -> r) winningRows
        || List.any (\r -> r) winningColumns


getMaybeWinningBoard : List MarkableBoard -> Maybe MarkableBoard
getMaybeWinningBoard boards =
    List.foldl
        (\board acc ->
            if isWinningBoard board then
                Just board

            else
                acc
        )
        Nothing
        boards


getWinningBoard : List Int -> List MarkableBoard -> ( MarkableBoard, Int )
getWinningBoard calledNumbers markableBoards =
    let
        firstNumber =
            Maybe.withDefault 0 <| List.head calledNumbers

        updatedBoards =
            markBoards firstNumber markableBoards

        maybeWinngingBoard =
            getMaybeWinningBoard updatedBoards

        restOfNumbers =
            Maybe.withDefault [] <| List.tail calledNumbers
    in
    case maybeWinngingBoard of
        Just board ->
            ( board, firstNumber )

        Nothing ->
            if List.length restOfNumbers > 0 then
                getWinningBoard restOfNumbers updatedBoards

            else
                ( [], 0 )


sumUnmarkedNumbers : MarkableBoard -> Int
sumUnmarkedNumbers board =
    List.foldl
        (\row acc ->
            acc
                + List.foldl
                    (\( num, marked ) acc1 ->
                        if marked then
                            acc1

                        else
                            acc1 + num
                    )
                    0
                    row
        )
        0
        board


getBingoFinalScore : List Int -> List Board -> Int
getBingoFinalScore calledNumbers boards =
    let
        markableBoards =
            getMarkableBoards boards

        ( winningBoard, lastNumber ) =
            Debug.log "winningBoard" (getWinningBoard calledNumbers markableBoards)
    in
    lastNumber * sumUnmarkedNumbers winningBoard


main : Html.Html msg
main =
    getBingoFinalScore [] []
        |> String.fromInt
        |> Html.text
