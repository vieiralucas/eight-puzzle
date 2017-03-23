module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Maybe.Extra
import Time exposing (Time)
import Board exposing (Board, Cell, Position)
import Search


-- MODEL


type alias Model =
    { board : Board
    , steps : List Matrix.Location
    }


initial : ( Model, Cmd Msg )
initial =
    let
        matrix =
            Matrix.fromList
                [ [ Cell (Position 0 0) 8, Cell (Position 1 0) 7, Cell (Position 2 0) 6 ]
                , [ Cell (Position 0 1) 5, Cell (Position 1 1) 4, Cell (Position 2 1) 3 ]
                , [ Cell (Position 0 2) 2, Cell (Position 1 2) 1, Cell (Position 2 2) 0 ]
                ]

        blank =
            Position 2 2

        board =
            { matrix = matrix, blank = blank }

        steps =
            Search.aStar board
    in
        ( { board = board, steps = steps }, Cmd.none )



-- UPDATE


type Msg
    = Move Cell
    | Step Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move cell ->
            ( { model | board = Board.move cell model.board }
            , Cmd.none
            )

        Step t ->
            case model.steps of
                [] ->
                    ( model, Cmd.none )

                step :: tail ->
                    let
                        newBoard =
                            Board.step step model.board

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


viewCell : Cell -> Html Msg
viewCell cell =
    if cell.v == 0 then
        Html.div [ style emptyStyle ] []
    else
        Html.div
            [ style cellStyle
            , onClick (Move cell)
            ]
            [ (viewValue cell.v) ]


viewRow : List Cell -> List (Html Msg)
viewRow row =
    List.map viewCell row


rowStyle : List ( String, String )
rowStyle =
    [ ( "display", "flex" ) ]


boardStyle : List ( String, String )
boardStyle =
    [ ( "border", "2px solid #ddd" )
    , ( "width", "606px" )
    ]


view : Model -> Html Msg
view model =
    let
        rows =
            model.board.matrix
                |> mapRows viewRow
                |> List.map (Html.div [ style rowStyle ])
    in
        Html.div [ style boardStyle ]
            rows


main =
    Html.program
        { init = initial
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Utils


mapRows : (List a -> List b) -> Matrix a -> List (List b)
mapRows mapper matrix =
    matrix
        |> Matrix.toList
        |> List.map mapper
