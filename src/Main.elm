module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Canvas exposing (Renderable)
import Color
import Dict exposing (Dict)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Task
import Types.CardinalPoint as CardinalPoint
    exposing
        ( CardinalPoint(..)
        , encodeCardinalPoint
        )
import Types.Maze as Maze exposing (Maze, MazeNode)
import Utils


apiHost : String
apiHost =
    "https://api.noopschallenge.com"



---- PATHBOT  ----


{-| Record type for pathbot api response

"Can't you come up with a better name?"

Nope.

-}
type alias Pathbot =
    { status : String
    , message : String
    , exits : List String
    , description : String
    , mazeExitDirection : String
    , mazeExitDistance : Int
    , locationPath : String
    }


decodePathbot : Decoder Pathbot
decodePathbot =
    Decode.succeed Pathbot
        |> required "status" Decode.string
        |> optional "message" Decode.string ""
        |> optional "exits" (Decode.list Decode.string) []
        |> required "description" Decode.string
        |> optional "mazeExitDirection" Decode.string ""
        |> optional "mazeExitDistance" Decode.int -1
        |> optional "locationPath" Decode.string ""



---- MODEL ----


type alias Model =
    { maze : Maze
    , moveDirection : CardinalPoint
    , moving : Bool
    , position : ( Int, Int )
    , width : Int
    , height : Int
    }


initialModel : Model
initialModel =
    { maze = Dict.empty
    , moveDirection = East
    , moving = True
    , position = ( -1, 0 )
    , width = 1
    , height = 1
    }


init : ( Model, Cmd Msg )
init =
    let
        windowSize { viewport } =
            ( round viewport.width, round viewport.height )
    in
    ( initialModel
    , Cmd.batch
        [ Http.post
            { url = apiHost ++ "/pathbot/start"
            , body = Http.emptyBody
            , expect = Http.expectJson GotPathbot decodePathbot
            }
        , Task.perform (Utils.uncurry ResizeWindow << windowSize) Dom.getViewport
        ]
    )



---- UPDATE ----


type Msg
    = GotPathbot (Result Http.Error Pathbot)
    | MovePlayer (Maybe CardinalPoint)
    | ResizeWindow Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPathbot result ->
            let
                updatedModel =
                    { model | moving = False }
            in
            case result of
                Err err ->
                    ( updatedModel, Cmd.none )

                Ok pathbot ->
                    ( updateMaze pathbot updatedModel, Cmd.none )

        MovePlayer movement ->
            let
                currentNode =
                    Dict.get model.position model.maze
            in
            if model.moving then
                update NoOp model

            else
                Maybe.map2
                    (\direction mazeNode ->
                        ( { model | moveDirection = direction, moving = True }
                        , postMove mazeNode.locationPath direction
                        )
                    )
                    movement
                    currentNode
                    |> Maybe.withDefault (update NoOp model)

        ResizeWindow width height ->
            ( { model | width = width, height = height }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


postMove : String -> CardinalPoint -> Cmd Msg
postMove path direction =
    Http.post
        { url = apiHost ++ path
        , body =
            [ ( "direction", encodeCardinalPoint direction ) ]
                |> Encode.object
                |> Http.jsonBody
        , expect = Http.expectJson GotPathbot decodePathbot
        }


updateMaze : Pathbot -> Model -> Model
updateMaze pathbot model =
    case pathbot.status of
        "in-progress" ->
            updateMazeInProgress pathbot model

        "finished" ->
            model

        _ ->
            model


updateMazeInProgress : Pathbot -> Model -> Model
updateMazeInProgress pathbot model =
    { model
        | position =
            CardinalPoint.toRelativeCoordinate
                model.moveDirection
                model.position
        , maze =
            Maze.insert
                model.moveDirection
                model.position
                (pathbot.exits
                    |> List.filterMap CardinalPoint.fromString
                    |> Maze.createNode pathbot.locationPath
                )
                model.maze
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown <| Decode.map MovePlayer decodeKey
        , Events.onResize ResizeWindow
        ]


decodeKey : Decoder (Maybe CardinalPoint)
decodeKey =
    Decode.map toCardinalPoint <| Decode.field "code" Decode.string


toCardinalPoint : String -> Maybe CardinalPoint
toCardinalPoint str =
    case str of
        "ArrowUp" ->
            Just North

        "KeyW" ->
            Just North

        "ArrowLeft" ->
            Just West

        "KeyA" ->
            Just West

        "ArrowDown" ->
            Just South

        "KeyS" ->
            Just South

        "ArrowRight" ->
            Just East

        "KeyD" ->
            Just East

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Canvas.toHtml
            ( model.width, model.height )
            []
            (renders model)
        ]


clearCanvas : ( Float, Float ) -> Renderable
clearCanvas ( width, height ) =
    Canvas.shapes
        [ Canvas.fill Color.white ]
        [ Canvas.rect ( 0, 0 ) width height ]


renders : Model -> List Renderable
renders model =
    let
        dimensions =
            ( toFloat model.width, toFloat model.height )

        ( offsetX, offsetY ) =
            model.position

        drawNode ( x, y ) node =
            drawMazeNode 15 dimensions ( x - offsetX, y - offsetY ) node
    in
    List.concat
        [ [ clearCanvas ( toFloat model.width, toFloat model.height ) ]
        , Dict.toList model.maze
            |> List.map (Utils.uncurry drawNode)
            |> List.concat
        ]


pointOnCanvas : Float -> ( Float, Float ) -> ( Int, Int ) -> ( Float, Float )
pointOnCanvas radius ( width, height ) ( x, y ) =
    ( radius * 4 * toFloat x + width / 2
    , radius * 4 * toFloat y + height / 2
    )


drawMazeNode : Float -> ( Float, Float ) -> ( Int, Int ) -> MazeNode -> List Renderable
drawMazeNode radius ( width, height ) ( x, y ) node =
    let
        red =
            Color.rgb255 236 67 66

        black =
            Color.rgb255 36 41 46

        fillColor =
            if ( x, y ) == ( 0, 0 ) then
                red

            else
                black

        getCanvasPoint =
            pointOnCanvas radius ( width, height )
    in
    [ Canvas.shapes
        [ Canvas.stroke black
        , Canvas.lineWidth 2
        ]
        (node
            |> Maze.toCardinalPoints
            |> List.map (Utils.flip CardinalPoint.toRelativeCoordinate ( x, y ))
            |> List.map
                (\( xx, yy ) ->
                    Canvas.path
                        (getCanvasPoint ( xx, yy ))
                        [ Canvas.lineTo (getCanvasPoint ( x, y )) ]
                )
        )
    , Canvas.shapes
        [ Canvas.fill fillColor ]
        [ Canvas.circle
            (getCanvasPoint ( x, y ))
            radius
        ]
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
