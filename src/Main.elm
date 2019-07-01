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


radius : Float
radius =
    15


gridUnit : Int
gridUnit =
    4



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
    , cameraX : Float
    , cameraY : Float
    }


initialModel : Model
initialModel =
    { maze = Dict.empty
    , moveDirection = East
    , moving = True
    , position = ( -1, 0 )
    , width = 1
    , height = 1
    , cameraX = 1
    , cameraY = 1
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
    | FrameUpdate Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPathbot result ->
            let
                _ =
                    Debug.log "pathbot" (Debug.toString result)

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

                doMove direction mazeNode =
                    let
                        nextPosition =
                            CardinalPoint.toRelativeCoordinate
                                direction
                                model.position

                        ( cameraX, cameraY ) =
                            updateCamera direction
                                ( model.cameraX, model.cameraY )

                        nextNodeExists =
                            Dict.member nextPosition model.maze
                    in
                    if nextNodeExists then
                        ( { model
                            | position = nextPosition
                            , cameraX = model.cameraX + cameraX
                            , cameraY = model.cameraY + cameraY
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | moveDirection = direction, moving = True }
                        , postMove mazeNode.locationPath direction
                        )
            in
            if model.moving then
                update NoOp model

            else
                Maybe.map2 doMove movement currentNode
                    |> Maybe.withDefault (update NoOp model)

        ResizeWindow width height ->
            ( { model
                | width = width
                , height = height
                , cameraX = toFloat width / 2
                , cameraY = toFloat height / 2
              }
            , Cmd.none
            )

        FrameUpdate deltaTime ->
            let
                midX =
                    toFloat model.width / 2

                midY =
                    toFloat model.height / 2

                camX =
                    model.cameraX

                camY =
                    model.cameraY
            in
            ( { model
                | cameraX = camX + (midX - camX) * deltaTime * 0.005
                , cameraY = camY + (midY - camY) * deltaTime * 0.005
              }
            , Cmd.none
            )

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
    let
        ( cameraX, cameraY ) =
            updateCamera model.moveDirection
                ( model.cameraX, model.cameraY )
    in
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
        , cameraX = model.cameraX + cameraX
        , cameraY = model.cameraY + cameraY
    }


updateCamera : CardinalPoint -> ( Float, Float ) -> ( Float, Float )
updateCamera direction model =
    let
        ( x, y ) =
            CardinalPoint.toCoordinate direction
    in
    ( radius * toFloat gridUnit * toFloat x
    , radius * toFloat gridUnit * toFloat y
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown <| Decode.map MovePlayer decodeKey
        , Events.onResize ResizeWindow
        , Events.onAnimationFrameDelta FrameUpdate
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
    div [] [ Canvas.toHtml ( model.width, model.height ) [] (renders model) ]


clearCanvas : ( Float, Float ) -> Renderable
clearCanvas ( width, height ) =
    Canvas.shapes
        [ Canvas.fill Color.white ]
        [ Canvas.rect ( 0, 0 ) width height ]


renders : Model -> List Renderable
renders model =
    let
        red =
            Color.rgb255 236 67 66

        ( offsetX, offsetY ) =
            model.position

        cameraPos =
            ( model.cameraX, model.cameraY )

        drawNode ( x, y ) node =
            drawMazeNode cameraPos ( x - offsetX, y - offsetY ) node
    in
    List.concat
        [ [ clearCanvas ( toFloat model.width, toFloat model.height ) ]
        , Dict.toList model.maze
            |> List.map (Utils.uncurry drawNode)
            |> List.concat
        , [ Canvas.shapes
                [ Canvas.fill red ]
                [ Canvas.circle
                    (pointOnCanvas cameraPos ( 0, 0 ))
                    (radius + 2)
                ]
          ]
        ]


pointOnCanvas : ( Float, Float ) -> ( Int, Int ) -> ( Float, Float )
pointOnCanvas ( cameraX, cameraY ) ( x, y ) =
    ( radius * toFloat gridUnit * toFloat x + cameraX
    , radius * toFloat gridUnit * toFloat y + cameraY
    )


drawMazeNode : ( Float, Float ) -> ( Int, Int ) -> MazeNode -> List Renderable
drawMazeNode ( cameraX, cameraY ) ( x, y ) node =
    let
        black =
            Color.rgb255 36 41 46

        getCanvasPoint =
            pointOnCanvas ( cameraX, cameraY )

        trimLine ( xx, yy ) =
            ( radius * toFloat (xx - x), radius * toFloat (yy - y) )

        drawLine ( xx, yy ) =
            Canvas.path
                (Utils.pointMap2Both (+)
                    (getCanvasPoint ( x, y ))
                    (trimLine ( xx, yy ))
                )
                [ Canvas.lineTo
                    (Utils.pointMap2Both (-)
                        (getCanvasPoint ( xx, yy ))
                        (trimLine ( xx, yy ))
                    )
                ]

        nextPoint direction =
            CardinalPoint.toRelativeCoordinate direction ( x, y )

        drawLineFromCardinal =
            drawLine << nextPoint

        drawUnvisted direction =
            Canvas.circle
                (getCanvasPoint <| nextPoint direction)
                (radius - 1)
    in
    [ Canvas.shapes
        [ Canvas.fill black ]
        [ Canvas.circle (getCanvasPoint ( x, y )) (radius - 1) ]
    , Canvas.shapes
        [ Canvas.stroke black
        , Canvas.lineWidth 2
        ]
        (List.foldl
            (\dir acc ->
                drawLineFromCardinal dir :: drawUnvisted dir :: acc
            )
            []
            (Maze.toCardinalPoints node)
        )
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
