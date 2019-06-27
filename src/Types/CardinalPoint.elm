module Types.CardinalPoint exposing (CardinalPoint(..), encodeCardinalPoint)

import Json.Encode as Encode exposing (Value)


type CardinalPoint
    = North
    | East
    | South
    | West
    | Northwest
    | Northeast
    | Southwest
    | Southeast


encodeCardinalPoint : CardinalPoint -> Value
encodeCardinalPoint direction =
    Encode.string <|
        case direction of
            North ->
                "N"

            East ->
                "E"

            South ->
                "S"

            West ->
                "W"

            Northwest ->
                "NW"

            Northeast ->
                "NE"

            Southwest ->
                "SW"

            Southeast ->
                "SE"
