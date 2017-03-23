module Board exposing (Board, Cell(..), Position, move, get)

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import List.Extra as List


type Cell
    = OccupiedCell Int
    | EmptyCell


type alias Position =
    ( Int, Int )


type alias Board =
    Dict Position Cell


neighbours : Position -> Board -> List ( Position, Cell )
neighbours ( x, y ) board =
    let
        leftPos =
            ( x - 1, y )

        left =
            ( leftPos, Dict.get leftPos board )

        rightPos =
            ( x + 1, y )

        right =
            ( rightPos, Dict.get rightPos board )

        topPos =
            ( x, y - 1 )

        top =
            ( topPos, Dict.get topPos board )

        botPos =
            ( x, y + 1 )

        bot =
            ( botPos, Dict.get botPos board )
    in
        [ top, bot, left, right ]
            |> List.map
                (\( pos, cell ) ->
                    case cell of
                        Just c ->
                            Just ( pos, c )

                        Nothing ->
                            Nothing
                )
            |> Maybe.values


set : Position -> Cell -> Board -> Board
set position cell board =
    Dict.insert
        position
        cell
        board


move : Position -> Board -> Board
move position board =
    let
        maybeCell =
            Dict.get position board
    in
        case maybeCell of
            Nothing ->
                board

            Just cell ->
                let
                    n =
                        neighbours position board

                    blank =
                        List.find (\( pos, cell ) -> cell == EmptyCell) n
                in
                    case blank of
                        Nothing ->
                            board

                        Just ( pos, blankCell ) ->
                            board
                                |> set pos cell
                                |> set position EmptyCell


get : Board -> Int -> Int -> Maybe Cell
get board x y =
    Dict.get ( x, y ) board
