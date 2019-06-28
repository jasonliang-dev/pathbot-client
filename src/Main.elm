module Main exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Types.CardinalPoint as CardinalPoint exposing (CardinalPoint(..), encodeCardinalPoint)
import Types.Maze as Maze exposing (Maze(..))


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
        |> optional "status" Decode.string ""
        |> optional "message" Decode.string ""
        |> optional "exits" (Decode.list Decode.string) []
        |> optional "description" Decode.string ""
        |> optional "mazeExitDirection" Decode.string ""
        |> optional "mazeExitDistance" Decode.int -1
        |> optional "locationPath" Decode.string ""



---- MODEL ----


type alias Model =
    { root : Maze
    }


initialModel : Model
initialModel =
    { root = Wall
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Http.post
        { url = apiHost ++ "/pathbot/start"
        , body = Http.emptyBody
        , expect = Http.expectJson GotPathbot decodePathbot
        }
    )



---- UPDATE ----


type Msg
    = GotPathbot (Result Http.Error Pathbot)
    | MovePlayer (Maybe CardinalPoint)
    | NoOp


postMove : String -> CardinalPoint -> Cmd Msg
postMove path direction =
    Http.post
        { url = apiHost ++ path
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "direction"
                      , encodeCardinalPoint direction
                      )
                    ]
        , expect = Http.expectJson GotPathbot decodePathbot
        }


updateModel : Pathbot -> Model -> Model
updateModel pathbot model =
    case pathbot.status of
        "in-progress" ->
            { root =
                pathbot.exits
                    |> List.map CardinalPoint.fromString
                    |> Maze.toMazeNode pathbot.locationPath
            }

        "finished" ->
            model

        _ ->
            model


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
                    ( model, Cmd.none )

                Ok pathbot ->
                    ( updateModel pathbot model, Cmd.none )

        MovePlayer probably ->
            case probably of
                Nothing ->
                    update NoOp model

                Just direction ->
                    case model.root of
                        Wall ->
                            update NoOp model

                        Undiscovered ->
                            update NoOp model

                        MazeNode rec ->
                            ( model
                            , postMove rec.locationPath direction
                            )

        NoOp ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown <| Decode.map MovePlayer decodeKey


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
        [ h1 [] [ text "Your Elm App is working!" ]
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
