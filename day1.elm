module Day1 exposing(..)

import Html exposing(text, div)
import List exposing( map
                    , map2
                    , foldl
                    , head
                    , reverse
                    , range
                    , repeat
                    , append
                    , tail
                    , member
                    )
import String exposing(left, dropLeft, toInt, split)

main = div []
       [ text (output directions |> toString)
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

calculateDistance : Int -> Int -> Int
calculateDistance x y = (abs x) + (abs y)

calculateDistanceCoords : Coordinates -> Int
calculateDistanceCoords coords =
    calculateDistance coords.x coords.y

inputCode = "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
directions = split ", " inputCode

output input =
    let
        movements = map makePair input
        (endpoint, changes) = foldl walk2 (coordinates, [(0,0)]) movements
    in
        changes
        --map (\(x, y) -> calculateDistance x y) changes

walk : Movement -> List Coordinates -> List Coordinates
walk movement coords = move movement (head coords
                                            |> Maybe.withDefault (Coordinates 0 0 North)) :: coords

repeats : List (Int, Int) -> List (Int, Int)
repeats moves =
    let
        elem = head moves |> Maybe.withDefault (0, 0)
        rest = tail moves |> Maybe.withDefault []
    in
        if member elem rest then
            elem :: (repeats rest)
        else
            repeats rest

walk2 : Movement -> (Coordinates, List (Int, Int)) -> (Coordinates, List (Int, Int))
walk2 movement (coords, list) =
    let
        newcoord = move movement coords
    in
        (newcoord, (append list (getIntermediateCoordinates newcoord.direction movement.amount coords)))

getIntermediateCoordinates : Direction -> Int -> Coordinates -> List (Int, Int)
getIntermediateCoordinates direction amount current =
    let
        zip x y = (x, y)
    in
        case direction of
            North -> map2 zip (repeat amount current.x) (differenceRange current.y (current.y + amount))
            East -> map2 zip (differenceRange current.x (current.x + amount)) (repeat amount current.y)
            South -> map2 zip (repeat amount current.x) (differenceRange current.y (current.y - amount))
            West -> map2 zip (differenceRange current.x (current.x - amount)) (repeat amount current.y)

differenceRange : Int -> Int -> List Int
differenceRange a b =
    if a > b then
        reverse (range b (a - 1))
    else
        range (a + 1) b

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
                (dropLeft 1 str
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
