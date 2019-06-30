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
import Types.Maze as Maze exposing (Maze)
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
    , position : ( Int, Int )
    , width : Int
    , height : Int
    }


initialModel : Model
initialModel =
    { maze = Dict.empty
    , moveDirection = East
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
        , Task.perform (ResizeWindow << windowSize) Dom.getViewport
        ]
    )



---- UPDATE ----


type Msg
    = GotPathbot (Result Http.Error Pathbot)
    | MovePlayer (Maybe CardinalPoint)
    | ResizeWindow ( Int, Int )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPathbot result ->
            let
                _ =
                    Debug.log "response" result
            in
            case result of
                Err err ->
                    update NoOp model

                Ok pathbot ->
                    ( updateMaze pathbot model, Cmd.none )

        MovePlayer movement ->
            let
                currentNode =
                    Dict.get model.position model.maze
            in
            Maybe.map2
                (\direction mazeNode ->
                    ( { model | moveDirection = direction }
                    , postMove mazeNode.locationPath direction
                    )
                )
                movement
                currentNode
                |> Maybe.withDefault (update NoOp model)

        ResizeWindow ( width, height ) ->
            ( { model | width = width, height = height }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


postMove : String -> CardinalPoint -> Cmd Msg
postMove path direction =
    Http.post
        { url = apiHost ++ path
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "direction", encodeCardinalPoint direction ) ]
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
        , Events.onResize (Utils.curry ResizeWindow)
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
    let
        nodeRadius =
            16

        position pos sizeFn =
            nodeRadius * 4 * toFloat pos + toFloat (sizeFn model) / 2

        pointToCircle ( x, y ) =
            Canvas.circle
                ( position x .width, position y .height )
                nodeRadius
    in
    div []
        [ Canvas.toHtml ( model.width, model.height )
            []
            [ clearCanvas ( toFloat model.width, toFloat model.height )
            , Canvas.shapes
                [ Canvas.fill Color.black ]
                (List.map pointToCircle <| Dict.keys model.maze)
            , Canvas.shapes
                []
                []
            ]
        ]


clearCanvas : ( Float, Float ) -> Renderable
clearCanvas ( width, height ) =
    Canvas.shapes
        [ Canvas.fill Color.white ]
        [ Canvas.rect ( 0, 0 ) width height ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
