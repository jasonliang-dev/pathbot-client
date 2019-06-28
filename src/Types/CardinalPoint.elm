module Types.CardinalPoint exposing (CardinalPoint(..), encodeCardinalPoint, fromString, toString)

import Json.Encode as Encode exposing (Value)
import Utils


type CardinalPoint
    = North
    | East
    | South
    | West
    | Northwest
    | Northeast
    | Southwest
    | Southeast


cardinalPoints : List ( String, CardinalPoint )
cardinalPoints =
    [ ( "N", North )
    , ( "E", East )
    , ( "S", South )
    , ( "W", West )
    , ( "NW", Northwest )
    , ( "NE", Northeast )
    , ( "SW", Southwest )
    , ( "SE", Southeast )
    ]


fromString : String -> Maybe CardinalPoint
fromString str =
    Utils.getSecond str cardinalPoints


toString : CardinalPoint -> String
toString direction =
    Utils.getFirst direction cardinalPoints
        -- getFirst lookup CANNOT FAIL
        -- But we need to satisfy the compiler
        |> Maybe.withDefault "N"


encodeCardinalPoint : CardinalPoint -> Value
encodeCardinalPoint direction =
    Utils.getFirst direction cardinalPoints
        |> Maybe.withDefault ""
        |> Encode.string
