module Types.CardinalPoint exposing
    ( CardinalPoint(..)
    , encodeCardinalPoint
    , fromString
    , toCoordinate
    , toRelativeCoordinate
    , toString
    )

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
        |> Maybe.withDefault "N"


toCoordinate : CardinalPoint -> ( Int, Int )
toCoordinate direction =
    case direction of
        North ->
            ( 0, -1 )

        South ->
            ( 0, 1 )

        East ->
            ( 1, 0 )

        West ->
            ( -1, 0 )

        Northeast ->
            ( 1, -1 )

        Northwest ->
            ( -1, -1 )

        Southeast ->
            ( 1, 1 )

        Southwest ->
            ( -1, 1 )


toRelativeCoordinate : CardinalPoint -> ( Int, Int ) -> ( Int, Int )
toRelativeCoordinate direction ( x, y ) =
    toCoordinate direction
        |> Tuple.mapBoth ((+) x) ((+) y)


encodeCardinalPoint : CardinalPoint -> Value
encodeCardinalPoint direction =
    Utils.getFirst direction cardinalPoints
        |> Maybe.withDefault ""
        |> Encode.string
