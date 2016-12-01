module Day1 exposing(..)

import Html exposing(text, div)
import List exposing(map, foldr)
import String exposing(left, right, toInt)

main = div []
       [ text (toString (output directions))
       ]

type alias Movement =
    { rotation: Rotation
    , amount: Int
    }

type alias Coordinates =
    { x: Int
    , y: Int
    , direction: Direction
    }

type Direction = North | South | East | West
type Rotation = Right | Left

coordinates = Coordinates 0 0 North

calculateDistance : Coordinates -> Int
calculateDistance coords =
    (abs coords.x) + (abs coords.y)

directions = ["L1", "L1", "L1", "L1"]

output input =
    let
        movements = map makePair input
    in
        foldr (\movement coord -> move movement coord) coordinates movements

move : Movement -> Coordinates -> Coordinates
move movement current =
    let
        newDirection = switchDirection current.direction movement.rotation
    in
        getNewCoordinates newDirection current movement.amount

getNewCoordinates : Direction -> Coordinates -> Int -> Coordinates
getNewCoordinates direction coords amount =
    case direction of
        North -> { coords | y = coords.y + amount, direction = direction }
        East -> { coords | x = coords.x + amount, direction = direction }
        South -> { coords | y = coords.y - amount, direction = direction }
        West -> { coords | x = coords.x - amount, direction = direction }

makePair : String -> Movement
makePair str = Movement
                (left 1 str
                    |> makeRotation)
                (right 1 str
                    |> toInt
                    |> Result.toMaybe
                    |> Maybe.withDefault 0)

makeRotation : String -> Rotation
makeRotation str =
    case str of
        "R" -> Right
        "L" -> Left
        _ -> Right

switchDirection : Direction -> Rotation -> Direction
switchDirection current rotation =
    case rotation of
        Right ->
            case current of
                North -> East
                East -> South
                South -> West
                West -> North
        Left ->
            case current of
                North -> West
                West -> South
                South -> East
                East -> North
