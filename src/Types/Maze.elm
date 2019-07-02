module Types.Maze exposing
    ( Maze
    , MazeNode
    , createNode
    , insert
    , singletonNode
    , toCardinalPoints
    )

import Dict exposing (Dict)
import Types.CardinalPoint as CardinalPoint exposing (CardinalPoint(..))


type alias MazeNode =
    { locationPath : String
    , north : Bool
    , south : Bool
    , east : Bool
    , west : Bool
    }


type alias Maze =
    Dict ( Int, Int ) MazeNode


type alias PlayableMaze a =
    { a
        | moveDirection : CardinalPoint
        , position : ( Int, Int )
        , maze : Maze
    }


singletonNode : MazeNode
singletonNode =
    { locationPath = ""
    , north = False
    , south = False
    , east = False
    , west = False
    }


createNode : String -> List CardinalPoint -> MazeNode
createNode locationPath directions =
    let
        addDirection direction node =
            case direction of
                North ->
                    { node | north = True }

                South ->
                    { node | south = True }

                East ->
                    { node | east = True }

                West ->
                    { node | west = True }

                _ ->
                    node
    in
    List.foldl addDirection
        { singletonNode | locationPath = locationPath }
        directions


toCardinalPoints : MazeNode -> List CardinalPoint
toCardinalPoints node =
    let
        get direction prop =
            if prop then
                Just direction

            else
                Nothing
    in
    List.filterMap identity
        [ get North node.north
        , get South node.south
        , get East node.east
        , get West node.west
        ]


insert : PlayableMaze a -> MazeNode -> Maze
insert model node =
    CardinalPoint.toRelativeCoordinate model.moveDirection model.position
        |> (\pos -> Dict.insert pos node model.maze)
