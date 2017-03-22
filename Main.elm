module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Matrix exposing (Matrix)


-- MODEL


type alias Cell =
    Maybe Int


type alias Model =
    Matrix Cell


initial : ( Model, Cmd Msg )
initial =
    let
        matrix =
            Matrix.fromList
                [ [ Just 8, Just 7, Just 6 ]
                , [ Just 5, Just 4, Just 3 ]
                , [ Just 2, Just 1, Nothing ]
                ]
    in
        ( matrix, Cmd.none )



-- UPDATE


type Msg
    = NothignYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



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
    case cell of
        Just v ->
            Html.div [ style cellStyle ] [ (viewValue v) ]

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
            model
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
