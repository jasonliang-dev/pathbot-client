module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Canvas exposing (Renderable)
import Color
import Dict
import Html exposing (Html)
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
        |> optional "description" Decode.string ""
        |> optional "mazeExitDirection" Decode.string ""
        |> optional "mazeExitDistance" Decode.int -1
        |> optional "locationPath" Decode.string ""



---- MODEL ----


type MazeFetchStatus
    = Failed
    | Loading
    | Loaded


type alias Model =
    { maze : Maze
    , fetchStatus : MazeFetchStatus
    , moveDirection : CardinalPoint
    , moving : Bool -- when True, ignore keyboard
    , position : ( Int, Int )
    , width : Int -- window width
    , height : Int -- window height
    , cameraX : Float
    , cameraY : Float
    , radius : Float -- can be though of as zoom size
    , defaultRadius : Float
    , finished : Bool
    }


initialModel : Model
initialModel =
    { maze = Dict.empty
    , fetchStatus = Loading
    , moveDirection = North
    , moving = True
    , position = ( 0, -1 )
    , width = 1
    , height = 1
    , cameraX = 1
    , cameraY = 1
    , radius = 80
    , defaultRadius = 14
    , finished = False
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
                baseModel =
                    if model.finished then
                        initialModel

                    else
                        model

                updatedModel =
                    { baseModel | moving = False }
            in
            case result of
                Err _ ->
                    ( { updatedModel | fetchStatus = Failed }, Cmd.none )

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
                Maybe.map2 (doMove model) movement currentNode
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

        FrameUpdate _ ->
            let
                tween initial final speed =
                    initial + (final - initial) * speed

                updatedCameraModel =
                    { model
                        | cameraX = tween model.cameraX (toFloat model.width / 2) 0.05
                        , cameraY = tween model.cameraY (toFloat model.height / 2) 0.05
                    }
            in
            if model.finished then
                let
                    finishUpModel =
                        { updatedCameraModel | radius = tween model.radius 1 0.025 }
                in
                case model.fetchStatus of
                    Failed ->
                        init

                    Loading ->
                        ( finishUpModel, Cmd.none )

                    Loaded ->
                        if model.radius < 1.5 then
                            init

                        else
                            ( finishUpModel, Cmd.none )

            else
                ( { updatedCameraModel
                    | radius = tween model.radius model.defaultRadius 0.08
                  }
                , Cmd.none
                )

        NoOp ->
            ( model, Cmd.none )


doMove : Model -> CardinalPoint -> MazeNode -> ( Model, Cmd Msg )
doMove model direction mazeNode =
    let
        nextPosition =
            CardinalPoint.toRelativeCoordinate
                direction
                model.position

        ( cameraX, cameraY ) =
            jumpCamera model.radius direction

        nextPathExists =
            Maze.toCardinalPoints mazeNode
                |> List.member direction

        nextNodeExists =
            Dict.member nextPosition model.maze
    in
    if not nextPathExists then
        ( model, Cmd.none )

    else if nextNodeExists then
        ( { model
            | position = nextPosition
            , cameraX = model.cameraX + cameraX
            , cameraY = model.cameraY + cameraY
          }
        , Cmd.none
        )

    else
        ( { model
            | moveDirection = direction
            , moving = True
            , fetchStatus = Loading
          }
        , postMove mazeNode.locationPath direction
        )


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
            finishMaze model

        _ ->
            model


updateMazeInProgress : Pathbot -> Model -> Model
updateMazeInProgress pathbot model =
    let
        updatedModel =
            applyPlayerMove model

        node =
            pathbot.exits
                |> List.filterMap CardinalPoint.fromString
                |> Maze.createNode pathbot.locationPath
    in
    { updatedModel
        | maze =
            Maze.insert model node
        , fetchStatus = Loaded
    }


finishMaze : Model -> Model
finishMaze model =
    let
        updatedModel =
            applyPlayerMove model
    in
    { updatedModel
        | maze =
            Maze.insert model Maze.singletonNode
        , finished = True
        , fetchStatus = Loaded
    }


applyPlayerMove : Model -> Model
applyPlayerMove model =
    let
        ( cameraX, cameraY ) =
            jumpCamera model.radius model.moveDirection
    in
    { model
        | position =
            CardinalPoint.toRelativeCoordinate
                model.moveDirection
                model.position
        , cameraX = model.cameraX + cameraX
        , cameraY = model.cameraY + cameraY
    }


jumpCamera : Float -> CardinalPoint -> ( Float, Float )
jumpCamera radius direction =
    let
        ( x, y ) =
            CardinalPoint.toCoordinate direction
    in
    ( radius * toFloat gridUnit * toFloat x
    , radius * toFloat gridUnit * toFloat y
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
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
    Html.div []
        [ Canvas.toHtml ( model.width, model.height ) [] (renders model) ]


clearCanvas : ( Float, Float ) -> Renderable
clearCanvas ( width, height ) =
    Canvas.shapes
        [ Canvas.fill Color.white ]
        [ Canvas.rect ( 0, 0 ) width height ]


renders : Model -> List Renderable
renders model =
    let
        alpha =
            if model.finished then
                max 0 (model.radius / model.defaultRadius)

            else
                model.defaultRadius / model.radius

        red =
            Color.rgba 0.92 0.26 0.26 alpha

        green =
            Color.rgba 0.32 0.64 0.32 alpha

        ( offsetX, offsetY ) =
            model.position

        cameraPos =
            ( model.cameraX, model.cameraY )

        drawNode ( x, y ) node =
            drawMazeNode model ( x - offsetX, y - offsetY ) node
    in
    List.concat
        [ [ clearCanvas ( toFloat model.width, toFloat model.height ) ]
        , Dict.toList model.maze
            |> List.map (Utils.uncurry drawNode)
            |> List.concat
        , [ Canvas.shapes
                [ Canvas.fill <|
                    if model.finished then
                        green

                    else
                        red
                ]
                [ Canvas.circle
                    (pointOnCanvas model.radius cameraPos ( 0, 0 ))
                    (model.radius + 2)
                ]
          ]
        ]


pointOnCanvas : Float -> ( Float, Float ) -> ( Int, Int ) -> ( Float, Float )
pointOnCanvas radius ( cameraX, cameraY ) ( x, y ) =
    ( radius * toFloat gridUnit * toFloat x + cameraX
    , radius * toFloat gridUnit * toFloat y + cameraY
    )


drawMazeNode : Model -> ( Int, Int ) -> MazeNode -> List Renderable
drawMazeNode model ( x, y ) node =
    let
        delta =
            Utils.pointMap2 (-)
                ( toFloat model.width / 2, toFloat model.height / 2 )
                (getCanvasPoint ( x, y ))

        fogAlpha =
            2 - Utils.pointMagnitude delta * 0.008

        alpha =
            if model.finished then
                min (model.radius / model.defaultRadius)
                    fogAlpha

            else
                clamp 0 (model.defaultRadius / model.radius) fogAlpha

        black =
            Color.rgba 0.14 0.16 0.18 alpha

        getCanvasPoint =
            pointOnCanvas model.radius ( model.cameraX, model.cameraY )

        trimLine ( xx, yy ) =
            ( model.radius * toFloat (xx - x)
            , model.radius * toFloat (yy - y)
            )

        drawLine ( xx, yy ) =
            Canvas.path
                (Utils.pointMap2 (+)
                    (getCanvasPoint ( x, y ))
                    (trimLine ( xx, yy ))
                )
                [ Canvas.lineTo
                    (Utils.pointMap2 (-)
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
                (model.radius - 1)
    in
    if alpha == 0 then
        []

    else
        [ Canvas.shapes
            [ Canvas.fill black ]
            [ Canvas.circle (getCanvasPoint ( x, y )) (model.radius - 1) ]
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
