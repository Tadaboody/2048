module Main exposing (..)

{-| Classic Snake game implemented in Elm, using elm-ui to display
its simple graphics.
-}

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as L
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import Random


type alias Position =
    { x : Int
    , y : Int
    }


type alias Tile =
    { value : Int
    , position : Position
    }


type alias Board =
    List Tile


type TileType
    = TileEmpty
    | TileFull


{-| All possible statuses of the game.
-}
type GameStatus
    = Playing
    | Lost String


{-| Simple tuple that indicates horizontal and vertical direction
as one of -1, 0, 1
-}
type alias Direction =
    ( Int, Int )


{-| Directions for the snake. Since we are dealing with a grid
and not pixel coordinates, it is easier to express the directions
as tuple for horizontal and vertical direction. Then we can apply
this to the snake update logic.
-}
type alias Directions =
    { right : Direction
    , left : Direction
    , up : Direction
    , down : Direction
    }


{-| Config holds defaults for the game.
-}
type alias Config =
    { fieldWidth : Int
    , fieldHeight : Int
    , tileSize : Int
    , initialBoard : Board
    }


{-| Set some reasonable defaults for the game.
-}
config : Config
config =
    { fieldWidth = 3
    , fieldHeight = 3
    , tileSize = 150
    , initialBoard = [ { value = 2, position = { x = 1, y = 1 } } ]
    }


directions : Directions
directions =
    { right = ( 1, 0 )
    , left = ( -1, 0 )
    , up = ( 0, -1 )
    , down = ( 0, 1 )
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { board : Board
    , gameStatus : GameStatus
    , score : Int
    }


type CollisionTestResult
    = NoCollision
    | HitTile (List Tile)
    | HitTheWall


type Msg
    = Move Direction
    | RandomPositions (List Position)
    | IgnoreKey


newModel : GameStatus -> Model
newModel gameStatus =
    { board = config.initialBoard
    , gameStatus = gameStatus
    , score = 0
    }


{-| Create a list of Positions
-}
randomPositions : Int -> Cmd Msg
randomPositions count =
    Random.generate RandomPositions <|
        Random.list count positionGenerator


{-| Create Position with random coordinates.
-}
positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2
        Position
        (Random.int 0 <| config.fieldWidth - 1)
        (Random.int 0 <| config.fieldHeight - 1)


init : () -> ( Model, Cmd Msg )
init _ =
    ( newModel Playing
    , randomPositions 1
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomPositions positions ->
            let
                newTile =
                    Tile 2

                newPositions =
                    diffList positions (List.map .position model.board)
            in
            if not <| List.isEmpty newPositions then
                ( { model
                    | board = model.board ++ List.map newTile newPositions
                  }
                , Cmd.none
                )

            else if isLost model.board then
                ( { model | gameStatus = Lost "" }, Cmd.none )

            else if boardFull model then
                ( model, Cmd.none )

            else
                ( model, randomPositions 1 )

        Move direction ->
            let
                movedModel =
                    { model | board = afterMove model.board direction }
            in
            if movedModel /= model then
                ( movedModel, randomPositions 1 )

            else
                ( model, Cmd.none )

        IgnoreKey ->
            ( model, Cmd.none )


boardFull : Model -> Bool
boardFull model =
    List.length model.board == config.fieldHeight * config.fieldHeight


isLost : Board -> Bool
isLost board =
    let
        sortBoard =
            List.sortWith cmpTile

        sortedEq =
            \first second -> sortBoard first == sortBoard second

        allDirections =
            [ directions.down, directions.up, directions.left, directions.right ]
    in
    List.all (sortedEq board) <| List.map (afterMove board) allDirections


cmpTile : Tile -> Tile -> Order
cmpTile first second =
    if first.position.x == second.position.x then
        compare first.position.y second.position.y

    else
        compare first.position.x second.position.x


groupByPos : Board -> List ( Tile, List Tile )
groupByPos board =
    List.sortWith cmpTile board |> List.Extra.groupWhile (\first second -> first.position == second.position)


bestValue : Tile -> Tile -> Tile
bestValue first second =
    if first.value > second.value then
        first

    else
        second


bestInGroup : ( Tile, List Tile ) -> Tile
bestInGroup ( a, l ) =
    List.foldl bestValue a l


withoutDups : Board -> Board
withoutDups board =
    groupByPos board |> List.map bestInGroup


nullTile : Tile
nullTile =
    Tile 0 (Position 0 0)


nonEmptyToEmpty : ( a, List a ) -> List a
nonEmptyToEmpty ( head, list ) =
    head :: list


mergeBoard : Board -> Board
mergeBoard board =
    let
        collisions =
            groupByPos board

        sumTiles =
            \first -> \second -> { first | value = first.value + second.value }

        sumGroup =
            \group -> List.Extra.foldl1 sumTiles group |> Maybe.withDefault nullTile
    in
    List.map sumGroup (List.map nonEmptyToEmpty collisions)


afterMove : Board -> Direction -> Board
afterMove board direction =
    let
        movedBoard =
            List.map (moveTile board direction) board |> mergeBoard |> withoutDups
    in
    if not <| movedBoard == board then
        -- If we moved we might move again.
        afterMove movedBoard direction

    else
        movedBoard


inBounds : Position -> Bool
inBounds position =
    position.x < config.fieldWidth && position.y < config.fieldHeight && position.x >= 0 && position.y >= 0


getCollisions : Board -> Position -> CollisionTestResult
getCollisions board position =
    if not <| inBounds position then
        HitTheWall

    else
        let
            collisions =
                List.filter (\t -> t.position == position) board
        in
        if not <| List.isEmpty collisions then
            HitTile collisions

        else
            NoCollision


moveTile : Board -> Direction -> Tile -> Tile
moveTile board direction tile =
    let
        newPos =
            movePosition tile.position direction

        collision =
            getCollisions board newPos

        movedTile =
            { tile | position = newPos }
    in
    case collision of
        HitTile (collidedTile :: _) ->
            if collidedTile.value == tile.value then
                movedTile

            else
                tile

        HitTheWall ->
            tile

        NoCollision ->
            movedTile

        -- Weird quirck of having a list of collisions, equivilant to NoCollision
        HitTile [] ->
            movedTile


movePosition : Position -> Direction -> Position
movePosition position ( x, y ) =
    { x = position.x + x, y = position.y + y }


diffList : List a -> List a -> List a
diffList aList bList =
    List.filter (\c -> not <| List.member c bList) aList



-- INPUTS


{-| Subscriptions to keyboard events and timer
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        ]


{-| Decoder for the pressed key
-}
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMessage (Decode.field "key" Decode.string)


{-| Classify pressed key and fire proper message.
-}
keyToMessage : String -> Msg
keyToMessage string =
    case string of
        "ArrowRight" ->
            Move directions.right

        "ArrowLeft" ->
            Move directions.left

        "ArrowUp" ->
            Move directions.up

        "ArrowDown" ->
            Move directions.down

        _ ->
            IgnoreKey



-- VIEW


{-| Browser.document requires the view to return Document type. So we
define the type alias for Document here and use it as a return type from
the view function.
-}
type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


{-| Entry point for the view. Here we want to set the main style of the game
page like the frame filling the entire page, background color and fancy font.
-}
view : Model -> Document Msg
view model =
    { title = "2048"
    , body =
        [ Element.layout
            [ Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Russo+One"
                    , name = "Russo One"
                    }
                , Font.sansSerif
                ]
            , Font.color gameColors.body
            , Background.color gameColors.frame1
            ]
          <|
            el
                (centerX :: styleGameFrame gameColors.frame1 gameColors.frame1 False)
                (L.lazy viewGame model)
        ]
    }


{-| Game colors. Lets put some meaning behind numbers.
-}
gameColors : { black : Color, yellow : Color, body : Color, snake : Color, tile : Color, food : Color, wall : Color, frame1 : Color, frame2 : Color, title : Color }
gameColors =
    { black = rgb255 0 0 0
    , yellow = rgb255 255 255 0
    , body = rgb255 255 255 255
    , snake = rgb255 0 220 0
    , tile = rgb255 50 20 20
    , food = rgb255 180 0 0
    , wall = rgb255 80 50 50
    , frame1 = rgb255 60 110 60
    , frame2 = rgb255 80 180 80
    , title = rgb255 0 220 0
    }


{-| Main view of the game
-}
viewGame : Model -> Element Msg
viewGame model =
    el (styleGameFrame gameColors.frame2 gameColors.black True) <|
        column
            ([ centerX, centerY, spacing 30 ]
                ++ viewMessage model.gameStatus
            )
        <|
            [ viewTitle
            , el [] <| viewField model
            , viewScore model.score
            ]


{-| View for both static and action field.
-}
viewField : Model -> Element Msg
viewField model =
    let
        width_ =
            config.fieldHeight * config.tileSize

        tileCount =
            config.fieldHeight * config.fieldWidth
    in
    el
        [ Border.color gameColors.wall
        , Border.width 15
        , Border.rounded 8
        ]
        (el (viewActionField model) <|
            L.lazy3 viewStaticField TileEmpty tileCount width_
        )


{-| Static field is just a playground UI that does not change. It serves
as a visual cue of the background.
-}
viewStaticField : TileType -> Int -> Int -> Element Msg
viewStaticField tileType count width_ =
    wrappedRow
        [ width <| px width_ ]
    <|
        List.map (\_ -> viewTile tileType Nothing) (List.repeat count 0)



-- {-| Overlay in front of the static field.
-- Here is where all the UI action happens.
-- -}


viewActionField : Model -> List (Element.Attribute Msg)
viewActionField model =
    let
        element =
            \a tileType -> Element.inFront <| viewTile tileType <| Just a
    in
    List.map (\x -> element x TileFull) model.board


{-| Show tile in specified position. When rendering static field, no position
is provided and the tile is displayed as a regular inline element.
-}
viewTile : TileType -> Maybe Tile -> Element Msg
viewTile tileType tile =
    el
        (styleTile tileType
            ++ (case tile of
                    Just t ->
                        [ moveRight <| toFloat (t.position.x * config.tileSize)
                        , moveDown <| toFloat (t.position.y * config.tileSize)
                        ]

                    Nothing ->
                        []
               )
        )
        (case tile of
            Just t ->
                Element.text <| String.fromInt t.value

            Nothing ->
                none
        )


{-| Title of the game
-}
viewTitle : Element Msg
viewTitle =
    el
        [ Font.color gameColors.title
        , Font.size 50
        , Font.bold
        , centerX
        ]
        (text "2048")


viewScore : Int -> Element Msg
viewScore score =
    row [ centerX ]
        [ el [ width <| px 100 ] (text "SCORE : ")
        , el [ width <| px 20 ] (text <| String.fromInt score)
        ]


viewMessage : GameStatus -> List (Element.Attribute Msg)
viewMessage status =
    case status of
        Lost string ->
            [ Element.inFront
                (el
                    [ centerX
                    , centerY
                    , padding 20
                    , Border.solid
                    , Border.color gameColors.body
                    , Border.width 1
                    , Border.rounded 5
                    , Background.color gameColors.black
                    ]
                 <|
                    column
                        [ spacing 10 ]
                        [ paragraph [ Font.center ] [ text string ]
                        , paragraph [ Font.center ] [ text "Lost" ]
                        ]
                )
            ]

        _ ->
            []


{-| Style of the tile, based on tile type
-}
styleTile : TileType -> List (Element.Attribute Msg)
styleTile tileContent =
    let
        tileColor =
            case tileContent of
                TileEmpty ->
                    gameColors.tile

                TileFull ->
                    gameColors.snake
    in
    [ width <| px config.tileSize
    , height <| px config.tileSize
    , Background.color tileColor
    , Border.color gameColors.black
    , Border.width 1
    , Border.solid
    , Border.rounded 3
    , Font.center
    , Font.bold
    , Font.size 150
    ]


{-| Helper function to make fancy border inside the game browser window.
-}
styleGameFrame : Element.Color -> Element.Color -> Bool -> List (Element.Attribute Msg)
styleGameFrame brcolor bgcolor rounded_ =
    [ width (Element.fill |> Element.maximum 700 |> Element.minimum 500)
    , height fill
    , Background.color bgcolor
    , Border.color brcolor
    , Border.width 10
    ]
        ++ (if rounded_ then
                [ Border.rounded 10 ]

            else
                []
           )
