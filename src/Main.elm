module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


apiHost : String
apiHost =
    "https://api.noopschallenge.com"


type MazeNode
    = Nil
    | MazeNode
        { north : MazeNode
        , east : MazeNode
        , south : MazeNode
        , west : MazeNode
        }


type CardinalPoint
    = North
    | East
    | South
    | West



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
    Pathbot


initialModel : Model
initialModel =
    { status = "init"
    , message = "hello world"
    , exits = []
    , description = "don't abandon this project like all the others"
    , mazeExitDirection = "N"
    , mazeExitDistance = 0
    , locationPath = ""
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
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPathbot result ->
            case result of
                Ok pathbot ->
                    ( pathbot, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



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
        , subscriptions = always Sub.none
        }
