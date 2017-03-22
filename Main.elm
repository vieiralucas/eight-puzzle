module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Maybe.Extra
import Board exposing (Board, Cell)


-- MODEL


type alias Model =
    { board : Board
    }


initial : ( Model, Cmd Msg )
initial =
    let
        board =
            Matrix.fromList
                [ [ Cell 0 0 (Just 8), Cell 1 0 (Just 7), Cell 2 0 (Just 6) ]
                , [ Cell 0 1 (Just 5), Cell 1 1 (Just 4), Cell 2 1 (Just 3) ]
                , [ Cell 0 2 (Just 2), Cell 1 2 (Just 1), Cell 2 2 Nothing ]
                ]
    in
        ( { board = board }, Cmd.none )



-- UPDATE


type Msg
    = Move Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move cell ->
            ( { model | board = Board.move cell model.board }
            , Cmd.none
            )



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
    case cell.v of
        Just v ->
            Html.div
                [ style cellStyle
                , onClick (Move cell)
                ]
                [ (viewValue v) ]

        Nothing ->
            Html.div [ style emptyStyle ] []


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
            model.board
                |> mapRows viewRow
                |> List.map (Html.div [ style rowStyle ])
    in
        Html.div [ style boardStyle ]
            rows


main =
    Html.program
        { init = initial
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- Utils


mapRows : (List a -> List b) -> Matrix a -> List (List b)
mapRows mapper matrix =
    matrix
        |> Matrix.toList
        |> List.map mapper
