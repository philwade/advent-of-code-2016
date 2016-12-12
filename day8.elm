module Day8 exposing(..)

import Html exposing(Html, text, div, span)
import Array exposing(Array)

{-
--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:

rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.
For example, here is a simple sequence on a smaller screen:

rect 3x2 creates a small rectangle in the top-left corner:

###....
###....
.......
rotate column x=1 by 1 rotates the second column down by one pixel:

#.#....
###....
.#.....
rotate row y=0 by 4 rotates the top row right by four pixels:

....#.#
###....
.#.....
rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:

.#..#.#
#.#....
.#.....
As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market. That's what the advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?

-}
partOneOutput = "hello 8"

type Instruction = Rect Int Int
                 | Column Int Int
                 | Row Int Int
                 | Noop

type Light = On | Off

sampleGrid = Array.repeat 3 ( Array.repeat 8 Off )

doInstruction : Instruction -> Array (Array Light) -> Array (Array Light)
doInstruction instruction grid =
    case instruction of
        Rect x y -> drawRect x y grid
        Column col amt ->
            rotateColumn col amt grid
        Row row amt ->
            rotateRow row amt grid
        Noop -> grid

rotateRow : Int -> Int -> Array (Array Light) -> Array (Array Light)
rotateRow rowIndex amount grid =
    let
        row = Array.get rowIndex grid
    in
        case row of
            Just r ->
                let
                    modulo = Array.length r
                    base = Array.repeat modulo Off
                    newRow = walkAndFillRow r modulo base amount
                in
                    Array.set rowIndex newRow grid

            Nothing -> grid

rotateColumn : Int -> Int -> Array (Array Light) -> Array (Array Light)
rotateColumn columnIndex amount grid =
    let
        column = getColumn columnIndex grid
        length = Array.length column
        base = Array.repeat length Off
        newColumn = walkAndFillRow column length base amount
    in
        setColumn grid columnIndex column

setColumn : Array (Array Light) -> Int -> Array Light -> Array (Array Light)
setColumn grid columnIndex newColumn =
    Array.indexedMap (\index row ->
                        case Array.get index newColumn of
                            Just x -> Array.set columnIndex x row
                            _ -> row) grid

getColumn : Int -> Array (Array Light) -> Array Light
getColumn index grid =
        Array.foldl (\row acc ->
                            case Array.get index row of
                                Just n -> Array.push n acc
                                _ -> Array.push Off acc) (Array.fromList []) grid

walkAndFillRow : Array Light -> Int -> Array Light -> Int -> Array Light
walkAndFillRow row length base amount =
    let
        (_, newRow) = Array.foldl (\item (index, base) -> case item of
                                                            On -> (index + 1, Array.set ((index + amount) % length) On base)
                                                            Off -> (index + 1, base)) (0, base) row
    in
        newRow

drawRect : Int -> Int -> Array (Array Light) -> Array (Array Light)
drawRect x y grid =
    Array.indexedMap (\index row -> if index < x then fillRow row y else row) grid

fillRow row amount = Array.append (Array.repeat amount On) (Array.slice 0 amount row)

renderGrid : Array (Array Light) -> Html msg
renderGrid grid =
    div [] (Array.map renderRow grid |> Array.toList)

renderRow : Array Light -> Html msg
renderRow row =
    div [] (Array.map renderLight row |> Array.toList)

renderLight : Light -> Html msg
renderLight light =
    case light of
        On -> text "#"
        Off -> text "."

main = String.split "\n" sampleInput
        |> List.map parseInstruction
        |> List.foldl doInstruction sampleGrid
        |> renderGrid

parseInstruction : String -> Instruction
parseInstruction rawIn =
    let
        parts = String.split " " rawIn
    in
        case List.head parts of
            Just "rect" ->
                parseRectangle parts
            Just "rotate" ->
                parseRotation parts
            Just _ -> Noop
            Nothing -> Noop

parseRectangle : List String -> Instruction
parseRectangle parts =
    let
        rest = List.tail parts |> Maybe.withDefault []
        rawShape = List.head rest |> Maybe.withDefault ""
        shapeParts = String.split "" rawShape
        x = List.head shapeParts |> Maybe.withDefault ""
        y = List.reverse shapeParts |> List.head |> Maybe.withDefault ""
    in
        case (String.toInt x, String.toInt y) of
            (Ok n, Ok m) ->
                Rect m n
            _ -> Noop

parseRotation : List String -> Instruction
parseRotation parts =
    let
        rest = List.tail parts |> Maybe.withDefault []
    in
        case List.head rest of
            Just "column" ->
                Column (parseAxis rest) (parseAmount rest)
            Just "row" ->
                Row (parseAxis rest) (parseAmount rest)
            _ -> Noop

parseAmount : List String -> Int
parseAmount rest =
    let
        amount = List.reverse rest |> List.head
    in
        case amount of
            Just n ->
                case String.toInt n of
                    Ok n -> n
                    _ -> 0
            _ -> 0

parseAxis : List String -> Int
parseAxis rest =
    let
        axis = List.tail rest |> Maybe.withDefault [] |> List.head
    in
        case axis of
            Just str ->
                let
                    strVal = String.split "" str |> List.reverse |> List.head
                in
                    case strVal of
                        Just n ->
                            case String.toInt n of
                                Ok n -> n
                                _ -> 0
                        _ -> 0
            Nothing -> 0

sampleInput = """rect 3x2
rotate column x=1 by 1"""
--rotate row y=0 by 4
--rotate column x=1 by 1"""
