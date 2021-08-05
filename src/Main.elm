module Main exposing (Direction(..), Model, Msg(..), main)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, p, table, text, th, tr)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)


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
    , board : List (List Tile)
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        -- "ArrowLeft" ->
        --     KeyPress Left

        -- "ArrowRight" ->
        --     KeyPress Right

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
      , board = [ [ Value 2, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text (String.fromInt model.score) ]
        , table [] (List.map renderRow model.board)
        ]


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
            ( { model | score = model.score + 1 }, Cmd.none )
