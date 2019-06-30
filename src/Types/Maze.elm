module Types.Maze exposing (Maze, createNode, insert)

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

        emptyNode =
            { locationPath = locationPath
            , north = False
            , south = False
            , east = False
            , west = False
            }
    in
    List.foldl addDirection emptyNode directions


insert : CardinalPoint -> ( Int, Int ) -> MazeNode -> Maze -> Maze
insert direction =
    Dict.insert << CardinalPoint.toRelativeCoordinate direction
