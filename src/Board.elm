module Board exposing (Board, Cell, Position, move, step)

import Matrix exposing (Matrix)
import Maybe.Extra


type alias Position =
    { x : Int
    , y : Int
    }


type alias Cell =
    { pos : Position
    , v : Int
    }


type alias Board =
    { matrix : Matrix Cell
    , blank : Position
    }


neighbours : Cell -> Board -> List Cell
neighbours cell { matrix } =
    let
        { x, y } =
            cell.pos

        left =
            Matrix.get (Matrix.loc y (x - 1)) matrix

        right =
            Matrix.get (Matrix.loc y (x + 1)) matrix

        top =
            Matrix.get (Matrix.loc (y - 1) x) matrix

        bot =
            Matrix.get (Matrix.loc (y + 1) x) matrix
    in
        Maybe.Extra.values [ top, bot, left, right ]


set : Cell -> Int -> Board -> Board
set cell newV board =
    let
        { x, y } =
            cell.pos

        matrix =
            Matrix.set
                (Matrix.loc y x)
                { cell | v = newV }
                board.matrix
    in
        { board | matrix = matrix }


manhattanDistance : Position -> Position -> Int
manhattanDistance pos1 pos2 =
    (abs (pos1.x - pos2.x)) + (abs (pos1.y - pos2.y))


swap : Cell -> Board -> Board
swap cell board =
    let
        blank =
            { pos = board.blank, v = 0 }

        newBoard =
            board
                |> set blank cell.v
                |> set cell 0
    in
        { newBoard | blank = cell.pos }


move : Cell -> Board -> Board
move cell board =
    let
        blank =
            board.blank

        distance =
            manhattanDistance cell.pos blank
    in
        if distance == 1 then
            -- this means blank is adjacent
            swap cell board
        else
            board


step : Matrix.Location -> Board -> Board
step location board =
    let
        maybeCell =
            Matrix.get location board.matrix
    in
        case maybeCell of
            Nothing ->
                board

            Just cell ->
                move cell board
