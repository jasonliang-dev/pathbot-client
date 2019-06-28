module Types.Maze exposing (Maze(..), MazeNodeRecord, toMazeNode, toMazeNodeHelper)

import Types.CardinalPoint as CardinalPoint exposing (CardinalPoint(..))

type alias MazeNodeRecord =
    { locationPath : String
    , north : Maze
    , east : Maze
    , south : Maze
    , west : Maze
    }


type Maze
    = Wall
    | Undiscovered
    | MazeNode MazeNodeRecord


toMazeNodeHelper : List (Maybe CardinalPoint) -> MazeNodeRecord -> Maze
toMazeNodeHelper directions acc =
    case directions of
        [] ->
            MazeNode acc

        x :: xs ->
            case x of
                Just North ->
                    toMazeNodeHelper xs { acc | north = Undiscovered }

                Just South ->
                    toMazeNodeHelper xs { acc | south = Undiscovered }

                Just East ->
                    toMazeNodeHelper xs { acc | east = Undiscovered }

                Just West ->
                    toMazeNodeHelper xs { acc | west = Undiscovered }

                _ ->
                    toMazeNodeHelper xs acc


toMazeNode : String -> List (Maybe CardinalPoint) -> Maze
toMazeNode locationPath xs =
    toMazeNodeHelper xs
        { locationPath = locationPath
        , north = Wall
        , east = Wall
        , south = Wall
        , west = Wall
        }
