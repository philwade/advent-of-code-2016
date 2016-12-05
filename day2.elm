module Day2 exposing(..)
import Html exposing(text, div)
import List exposing(map, foldl, reverse, tail, head)

main = div []
       [ text (allLines directions
                |> reverse
                |> tail
                |> Maybe.withDefault []
                |> map (getGridValue grid)
                |> toString)
       ]

type Direction = Up
               | Down
               | Left
               | Right
               | Nothing

type GridValue = Value String
               | None


parseDirection : String -> Direction
parseDirection str =
    case str of
        "U" -> Up
        "D" -> Down
        "L" -> Left
        "R" -> Right
        _ -> Nothing

sample = "UUURRRRULRDLRDRRDURDDDLLDLLLULDUDDLDLULUURULRLDLRRLLLRRDRRLDDLLULUDUDDLRDRDUURDLURUURLRULLDDURULRRURDUURLULUUUURDDDDUUDLULRULLLRLLRRRURDLLRLLRRRUURULRDRUUDDDDDLLLRURRURRUURDUURDDRDLULRRLLLDRRRLURRLLURLDRRDDLDLRRLLRDRLLLLDLULDLRRDRRLDDURLULLUDLUDRRDRRLRLULURDRLRLUUUDLRLDLLLURDUDULULDDRUUURLLLDLLDDUDDRURURUDDLUULRDRRRRLDRDDURLUDURDULLDLUDLULDRLRLLRLLLLRURDURLLDRRDRLRUUUUULLLRUDURUDLLLUDLLLLRDLDRDUDRURLUDDUDDURLUUUUDDLLUDLULLLLLDUDLLRLRRDDDULULRLDRLLULDLUDLLURULRDDUURULRDLDLDLRL URUUURDULUDLUUUUDDRRRDRRRLDUDLRDRRDRDDLRUULDLLDUDULLLRLDRDRRLDLDLUUDRUULDUDULDUDURURDDURULDLURULRLULDUDDUULDLLLDDURDDRDDURUULUUDRLDDULDRRRRDURRUDLLLURDDDLRULLRDDRDDDDLUUDRDUULRRRRURULDDDLDDRDRRUDRRURUDRDDLDRRRLLURURUULUUDRDULLDRLRDRRDDURDUDLDRLUDRURDURURULDUUURDUULRRRRRUDLLULDDDRLULDDULUDRRRDDRUDRRDLDLRUULLLLRRDRRLUDRUULRDUDRDRRRDDRLLRUUDRLLLUDUDLULUUDULDRRRRDDRURULDULLURDLLLDUUDLLUDRLDURRRLDDDURUDUDURRULDD LRUDDULLLULRLUDUDUDRLLUUUULLUDLUUUUDULLUURDLLRDUDLRUDRUDDURURRURUDLLLRLDLUDRRRRRRDLUURLRDDDULRRUDRULRDRDDUULRDDLDULDRRRDDLURRURLLLRURDULLRUUUDDUDUURLRLDDUURLRDRRLURLDRLLUUURDRUUDUUUDRLURUUUDLDRRLRLLRRUURULLLRLLDLLLDULDDLDULDLDDRUDURDDURDUDURDLLLRRDDLULLLUDURLUDDLDLUUDRDRUDUUDLLDDLLLLDRDULRDLDULLRUDDUULDUDLDDDRUURLDRRLURRDDRUUDRUDLLDLULLULUDUDURDDRLRDLRLDRLDDRULLLRUDULDRLRLRULLRLLRRRLLRRRDDRULRUURRLLLRULDLUDRRDDLLLUDDUDDDLURLUDRDLURUUDLLDLULURRLLDURUDDDDRLULRDDLRLDDLRLLDDRRLRDUDUUULRRLRULUDURDUDRLRLRUDUDLLRRRRLRRUDUL RULLLLUUUDLLDLLRULLRURRULDDRDLUULDRLLRUDLLRRLRDURLLDUUUUURUUURDLUURRLDDDLRRRRLRULDUDDLURDRRUUDLRRRDLDDUDUDDRUDURURLDULLDLULDLLUDLULRDRLLURRLLDURLDLRDLULUDDULDLDDDDDUURRDRURLDLDULLURDLLDDLLUDLDLDRLRLDLRDRLDLRRUUDRURLUUUDLURUULDUDRDULLDURUDLUUURRRLLDUDUDDUUULLLRUULDLURUDDRLUDRDDLDLLUDUDRRRDDUUULUULLLRLLUURDUUDRUUULULLDLDRUUDURLLUULRLDLUURLLUUDRURDDRLURULDUDUUDRRUUURDULRLDUUDDRURURDRRULDDDRLUDLLUUDURRRLDLRLRDRURLURLLLRLDDLRRLDLDDURDUUDRDRRLDRLULDRLURUUUDDRLLLDDLDURLLLLDRDLDRRUDULURRLULRDRLLUULLRLRDRLLULUURRUDRUDDDLLDURURLURRRDLLDRDLUDRULULULRLDLRRRUUDLULDURLRDRLULRUUURRDDLRUURUDRURUDURURDD DURRDLLLDDLLDLLRLULULLRDLDRRDDRDLRULURRDUUDDRLLDDLDRRLRDUDRULDLRURDUUDRDDLLDRRDRUDUDULLDDDDLDRRRLRLRDRDLURRDDLDDDUUDRDRLLLDLUDDDLUULRDRLLLRLLUULUDDDRLDUUUURULRDDURRDRLUURLUDRLRLLLDDLRDDUULRRRRURDLDDDRLDLDRRLLDRDDUDDUURDLDUUDRDLDLDDULULUDDLRDDRLRLDDLUDLLDRLUDUDDRULLRLDLLRULRUURDDRDRDRURDRRLRDLLUDDRRDRRLDDULLLDLUDRRUDLDULDRURRDURLURRLDLRDLRUDLULUDDRULRLLDUURULURULURRLURRUULRULRRRLRDLULRLRLUDURDDRUUURDRLLRRRDDLDRRRULLDLRDRULDRRLRRDLUDDRDDDUUURRLULLDRRUULULLRRRRLDDRDDLUURLLUDLLDUDLULUULUDLLUUURRRUDDDRLLLRDRUUDUUURDRULURRLRDLLUURLRDURULDRRUDURRDDLDRLDRUUDRLLUDLRRU"
lines = String.split " " sample
splitDirections = String.split ""
directions = map (\x -> splitDirections x |> List.map parseDirection) lines

singleLine : List Direction -> (Int, Int) -> (Int, Int)
singleLine line startingpoint = foldl (\direction position -> move2 direction position) startingpoint line

allLines : List (List Direction) -> List (Int, Int)
allLines directions = foldl (\line stops -> (singleLine line (List.head stops |> Maybe.withDefault (0,0))) :: stops) [(0, 2)] directions

gridTranslate : (Int, Int) -> Int
gridTranslate (x, y) =
   case (x, y) of
        (-1, 1) -> 1
        (0, 1) -> 2
        (1, 1) -> 3
        (-1, 0) -> 4
        (0, 0) -> 5
        (1, 0) -> 6
        (-1, -1) -> 7
        (0, -1) -> 8
        (1, -1) -> 9
        _ -> 0

move : Direction -> (Int, Int) -> (Int, Int)
move direction (x, y) =
    case direction of
        Up -> (x, (addTo y))
        Down -> (x, (subtractFrom y))
        Left -> ((subtractFrom x), y)
        Right -> ((addTo x), y)
        Nothing -> (x, y)

move2 : Direction -> (Int, Int) -> (Int, Int)
move2 direction (x, y) =
    let
        newPosition = case direction of
                Up -> (x, y - 1)
                Down -> (x, y + 1)
                Left -> (x - 1, y)
                Right -> (x + 1, y)
                Nothing -> (x, y)
        newValue = getGridValue grid newPosition
    in
        case newValue of
            None -> (x, y)
            Value _ -> newPosition


grid : List (List GridValue)
grid = [ [ None,      None,      Value "1", None,      None ]
       , [ None,      Value "2", Value "3", Value "4", None ]
       , [ Value "5", Value "6", Value "7", Value "8", Value "9" ]
       , [ None,      Value "A", Value "B", Value "C", None ]
       , [ None,      None,      Value "D", None,      None ]
       ]

getGridValue : List (List GridValue) -> (Int, Int) -> GridValue
getGridValue grid (x,y) = getGridRow y 0 grid |> getGridColumn x 0

getGridRow : Int -> Int -> List (List GridValue) -> List GridValue
getGridRow index current grid =
    if index == current then
        head grid |> Maybe.withDefault [None]
    else
        getGridRow index (current + 1) (tail grid |> Maybe.withDefault [[None]])

getGridColumn : Int -> Int -> List GridValue -> GridValue
getGridColumn index current grid =
    if index == current then
        head grid |> Maybe.withDefault None
    else
        getGridColumn index (current + 1) (tail grid |> Maybe.withDefault [None])

addTo : Int -> Int
addTo n =
    if n < 1 then
        n + 1
    else n

subtractFrom : Int -> Int
subtractFrom n =
    if n > -1 then
        n - 1
    else
        n
