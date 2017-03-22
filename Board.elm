module Board exposing (Board, Cell, move)

import Matrix exposing (Matrix)
import Maybe.Extra


type alias Cell =
    { x : Int
    , y : Int
    , v : Maybe Int
    }


type alias Board =
    Matrix Cell


neighbours : Cell -> Board -> List Cell
neighbours cell board =
    let
        left =
            Matrix.get (Matrix.loc cell.y (cell.x - 1)) board

        right =
            Matrix.get (Matrix.loc cell.y (cell.x + 1)) board

        top =
            Matrix.get (Matrix.loc (cell.y - 1) cell.x) board

        bot =
            Matrix.get (Matrix.loc (cell.y + 1) cell.x) board
    in
        Maybe.Extra.values [ top, bot, left, right ]


move : Cell -> Board -> Board
move cell board =
    let
        n =
            neighbours cell board

        isBlank c =
            Maybe.Extra.isNothing c.v

        blank =
            List.filter isBlank n
    in
        case blank of
            [] ->
                board

            blank :: _ ->
                board
                    |> Matrix.set (Matrix.loc blank.y blank.x) { blank | v = cell.v }
                    |> Matrix.set (Matrix.loc cell.y cell.x) { cell | v = Nothing }
