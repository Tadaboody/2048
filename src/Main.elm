module Main exposing (Direction(..), Model, Msg(..), main)

import Browser
import Html exposing (Html, table, text, th, tr)
import Html.Attributes exposing (style)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { score : Int
    , board : List (List Tile)
    }


type Tile
    = Empty
    | Value Int


init : Model
init =
    { score = 0
    , board = [ [ Value 2, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
    }


view : Model -> Html Msg
view model =
    table [] (List.map renderRow model.board)


renderRow : List Tile -> Html Msg
renderRow row =
    tr [] (List.map renderTile row)


renderTile : Tile -> Html Msg
renderTile tile =
    case tile of
        Empty ->
            th [style "width" "20"] [ text "Empty" ]

        Value value ->
            th [] [ text (String.fromInt value) ]


type Direction
    = Up
    | Down
    | Left
    | Right


type Msg
    = KeyPress Direction


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyPress _ ->
            model
