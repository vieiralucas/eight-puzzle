module Main exposing (..)

import Board exposing (Board, Cell, Position)
import Dict
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Search
import Time exposing (Time)


-- MODEL


type alias Model =
    { board : Board
    , rows : Int
    , cols : Int
    , steps : List Board.Position
    }


initial : ( Model, Cmd Msg )
initial =
    let
        board =
            Dict.fromList
                [ ( ( 0, 0 ), (Board.OccupiedCell 8) )
                , ( ( 1, 0 ), (Board.OccupiedCell 7) )
                , ( ( 2, 0 ), (Board.OccupiedCell 6) )
                  -- Row 0
                , ( ( 0, 1 ), (Board.OccupiedCell 5) )
                , ( ( 1, 1 ), (Board.OccupiedCell 4) )
                , ( ( 2, 1 ), (Board.OccupiedCell 3) )
                  -- Row 1
                , ( ( 0, 2 ), (Board.OccupiedCell 2) )
                , ( ( 1, 2 ), (Board.OccupiedCell 1) )
                , ( ( 2, 2 ), Board.EmptyCell )
                  -- Row 2
                ]

        steps =
            Search.aStar board
    in
        ( { board = board, steps = steps, rows = 3, cols = 3 }, Cmd.none )



-- UPDATE


type Msg
    = Move Position
    | Step Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move position ->
            ( { model | board = Board.move position model.board }
            , Cmd.none
            )

        Step t ->
            case model.steps of
                [] ->
                    ( model, Cmd.none )

                step :: tail ->
                    let
                        newBoard =
                            Board.move step model.board

                        newModel =
                            { model | steps = tail, board = newBoard }
                    in
                        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (600 * Time.millisecond) Step



-- VIEW


viewValue : Int -> Html Msg
viewValue v =
    Html.text (toString v)


cellStyle : List ( String, String )
cellStyle =
    [ ( "width", "200px" )
    , ( "height", "170px" )
    , ( "padding-top", "30px" )
    , ( "background", "cornsilk" )
    , ( "border", "2px solid #ddd" )
    , ( "font-size", "120px" )
    , ( "text-align", "center" )
    ]


emptyStyle : List ( String, String )
emptyStyle =
    [ ( "width", "200px" )
    , ( "height", "200px" )
    , ( "border", "2px solid #ddd" )
    ]


viewCell : ( Board.Position, Cell ) -> Html Msg
viewCell ( pos, cell ) =
    case cell of
        Board.OccupiedCell v ->
            Html.div
                [ style cellStyle
                , onClick (Move pos)
                ]
                [ (viewValue v) ]

        Board.EmptyCell ->
            Html.div [ style emptyStyle ] []


viewRow : List ( Board.Position, Cell ) -> Html Msg
viewRow row =
    row
        |> List.map viewCell
        |> Html.div [ style rowStyle ]


rowStyle : List ( String, String )
rowStyle =
    [ ( "display", "flex" ) ]


boardStyle : List ( String, String )
boardStyle =
    [ ( "border", "2px solid #ddd" )
    , ( "width", "606px" )
    ]


viewBoard : ( Int, Int ) -> Board -> Html Msg
viewBoard ( rows, cols ) board =
    let
        posCell row col =
            ( ( row, col ), (Dict.get ( row, col ) board) )

        toListOfMaybe posCells =
            posCells
                |> List.map
                    (\( pos, maybeCell ) ->
                        case maybeCell of
                            Nothing ->
                                Nothing

                            Just cell ->
                                Just ( pos, cell )
                    )

        makeRow row =
            Html.div [ style rowStyle ]
                [ List.range 0 (cols - 1)
                    |> List.map (posCell row)
                    |> toListOfMaybe
                    |> Maybe.values
                    |> viewRow
                ]
    in
        List.range 0 (rows - 1)
            |> List.map makeRow
            |> Html.div [ style boardStyle ]


view : Model -> Html Msg
view model =
    model.board
        |> viewBoard ( model.rows, model.cols )


main =
    Html.program
        { init = initial
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
