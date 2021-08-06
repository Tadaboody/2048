module Main exposing (Direction(..), Model, Msg(..), main)

import Array2D
import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
import Html exposing (Html, div, p, table, text, th, tr)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List.Extra


main : Program Inputs Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { score : Int
    , board : Board
    }


type alias Board =
    Array2D.Array2D Tile


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            KeyPress Left

        "ArrowRight" ->
            KeyPress Right

        _ ->
            KeyPress Other


type Tile
    = Empty
    | Value Int


type alias Inputs =
    {}


init : Inputs -> ( Model, Cmd Msg )
init _ =
    ( { score = 0
      , board = Array2D.fromList [ [ Value 2, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text (String.fromInt model.score) ]
        , table [] (Array2D.map renderRow model.board)
        ]

getArrays = 


renderRow : List Tile -> Html Msg
renderRow row =
    tr [] (List.map renderTile row)


renderTile : Tile -> Html Msg
renderTile tile =
    case tile of
        Empty ->
            th [ style "width" "20" ] [ text "Empty" ]

        Value value ->
            th [] [ text (String.fromInt value) ]


type Direction
    = Up
    | Down
    | Left
    | Right
    | Other


type Msg
    = KeyPress Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress _ ->
            ( { model | score = model.score + 1, board = updateBoard model.board (Move { x = 0, y = 0 } { x = 3, y = 3 }) }, Cmd.none )



-- Game


type alias Position =
    { x : Int, y : Int }


type BoardUpdate
    = Move Position Position
    | Merge Position Position



-- Helper function to unwrap a value you're sure is just ;)


sure : Maybe Tile -> Tile
sure maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Value -1


updateBoard : Board -> BoardUpdate -> Board
updateBoard board board_update =
    case board_update of
        Move from to ->
            Array2D.set to.x to.y (sure (Array2D.get from.x from.y board)) board

        Merge from to ->
            Array2D.set to.x to.y (Value -6) board
